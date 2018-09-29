import typing as t
import typing_extensions as tx
from functools import singledispatch
from dictknife import loading


class reify(object):
    """cached property"""

    def __init__(self, wrapped):
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except Exception:
            pass

    def __get__(self, inst, objtype=None):
        if inst is None:
            return self
        val = self.wrapped(inst)
        setattr(inst, self.wrapped.__name__, val)
        return val


@singledispatch
def guess_type(o):
    raise TypeError(f"{o!r} is not supported")


guess_type.register(int, lambda o: "integer")
guess_type.register(str, lambda o: "string")


def get_resolver():
    global DEFAULT_RESOLVER
    return DEFAULT_RESOLVER


class XRefStrategy:
    def __init__(self, lookup):
        self.lookup = lookup


class Lookup:
    def __init__(self, resolver):
        self.resolver = resolver
        self._cache = {}

    def lookup(self, ns: "Namespace", query: str) -> t.Optional["Member"]:
        """query is /foo/bar/boo"""
        path = tuple(query.strip("/").split("/"))  # todo: ~1

        item = self._cache.get(path)
        if item is not None:
            return item

        for k in path[:-1]:
            ns = ns.children[k]
        name = path[-1]
        for m in ns.members:
            if m.get_name() == name:
                self._cache[path] = m
                return m
        return None


class Resolver:
    def __init__(self, xref_strategy_factory=None, lookup=None):
        self.lookup = lookup or Lookup(self)
        self.xref_strategy = (xref_strategy_factory or XRefStrategy)(lookup)

    @reify
    def _schema_ignore_props_set(self):
        return set(Object.__dict__.keys())

    def resolve_name(self, cls):
        return cls.get_name()

    def resolve_description(self, cls):
        return cls.__doc__

    def resolve_object_properties(self, cls, *, history: t.Sequence["Member"]) -> t.Dict:
        properties = {}
        for target in cls.mro():
            for k, v in target.__dict__.items():
                if k in properties:
                    continue
                if k in self._schema_ignore_props_set:
                    continue
                if k.startswith("_"):
                    continue
                properties[k] = self.resolve_field(v, history=history)
        return properties

    def resolve_array_items(self, cls, *, history: t.Sequence["Member"]) -> t.Dict:
        return self.resolve_type(cls.items, history=history)

    def resolve_field(self, v, *, history: t.Sequence["Member"]) -> t.Dict:
        if hasattr(v, "as_dict"):
            return self.resolve_type(v, history=history)
        # xxx:
        return {"type": self.resolve_type(v, history=history)}

    def resolve_type(self, v, *, history: t.Sequence["Member"]) -> t.Dict:
        if not hasattr(v, "as_dict"):
            return guess_type(v)

        ref = getattr(v, "_ref", None)  # xxx
        if ref:
            return ref.as_dict(resolver=self, history=history)

        ns_list = []
        for m in history:
            if is_namespace(m):
                ns_list.append(m)
        ref = Ref(v, ns_list=ns_list)
        v._ref = ref  # xxx: cache
        return ref.as_dict(resolver=self, history=history)


DEFAULT_RESOLVER = Resolver()


# todo: ref
class Member(tx.Protocol):
    def get_name(self) -> str:
        ...

    def on_mount(self, ns: "Namespace") -> "Member":
        ...

    def as_dict(
        self,
        *,
        resolver: t.Optional[Resolver] = None,
        history: t.Sequence["Member"] = None,
    ) -> t.Dict:
        ...


class Ref:
    def __init__(self, o: Member, ns_list: t.Sequence[Member]) -> None:
        self.o = o
        self.ns_list = ns_list

    @reify
    def fullpath(self):
        nodes = [*self.ns_list, self.o]
        return "#/" + "/".join([m.get_name() for m in nodes])

    def as_dict(
        self,
        *,
        resolver: t.Optional[Resolver] = None,
        history: t.Sequence[Member] = None,
    ) -> t.Dict:
        return {"$ref": self.fullpath}


class Object:
    @classmethod
    def get_name(cls) -> str:
        return cls.__name__

    @classmethod
    def on_mount(cls, ns: "Namespace") -> "Member":
        return cls

    @classmethod
    def as_dict(
        cls,
        *,
        resolver: t.Optional[Resolver] = None,
        history: t.Sequence[Member] = None,
    ) -> t.Dict:
        r = resolver or get_resolver()
        h = (history or []) + [cls]

        d = {}
        d["type"] = "object"

        description = r.resolve_description(cls)
        if description:
            d["description"] = description

        properties = r.resolve_object_properties(cls, history=h)
        if properties:
            d["properties"] = properties
        return d


class _Alias:
    def __init__(self, o: Member, *, name: str):
        self.o = o
        self.name = name

    def get_name(self) -> str:
        return self.name

    def __getattr__(self, name: str) -> t.Any:
        return getattr(self.o, name)


class Array:
    @classmethod
    def get_name(cls) -> str:
        return cls.__name__

    @classmethod
    def on_mount(cls, ns: "Namespace") -> "Member":
        if cls.items not in ns:
            ns.mount(cls.items)
        return cls

    @classmethod
    def as_dict(
        cls,
        *,
        resolver: t.Optional[Resolver] = None,
        history: t.Sequence[Member] = None,
    ) -> t.Dict:
        r = resolver or get_resolver()
        h = (history or []) + [cls]

        d = {}
        d["type"] = "array"

        description = r.resolve_description(cls)
        if description:
            d["description"] = description

        d["items"] = r.resolve_array_items(cls, history=h)
        return d


class Namespace:
    name: str

    def __init__(self, name: str, ns=None) -> None:
        self.name = name
        self.members = []
        self._seen = set()
        self.children = {}  # sub ns
        self.ns = ns  # parent
        if ns is not None:
            ns.mount(self)

    def __contains__(self, schema):
        if schema in self._seen:
            return True
        for ns in self.children.values():
            if schema in ns:  # todo: cache?
                return True
        return False

    def get_name(self) -> str:
        return self.name

    def on_mount(self, ns: "Namespace") -> "Member":
        copied = self.__class__(self.name, ns=None)
        copied.ns = ns
        copied.members = self.members
        copied._seen = self._seen
        copied.children = self.children
        return copied

    def mount(self, member: t.Any, *, force=False, name=None) -> None:
        if member in self._seen:
            return
        if name is not None:
            member = _Alias(member, name=name)
        self._seen.add(member)
        self.members.append(member.on_mount(self))
        if is_namespace(member):
            self.children[member.get_name()] = member

    def namespace(self, name) -> "Namespace":  # todo: subtyping(type definition)
        ns = self.children.get(name)
        if ns is not None:
            return ns
        return self.__class__(name, ns=self)

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        pass

    def as_dict(
        self,
        *,
        resolver: t.Optional[Resolver] = None,
        history: t.Sequence[Member] = None,
    ) -> t.Dict:
        r = resolver or get_resolver()
        history = (history or []) + [self]
        d = {r.resolve_name(m): m.as_dict(resolver=r, history=history) for m in self.members}
        if self.ns is not None:
            return d
        return {self.name: d}


def is_namespace(ns) -> bool:
    return hasattr(ns, "mount")


class Person(Object):
    """person"""

    name: str = "foo"
    age: int = 20


class XPerson(Object):
    """X person"""
    x: str = "x"
    person: Person = Person  # xxx:


class PersonWithNickname(Person):
    """Y person"""
    nickname: str = "str"


class People(Array):
    items = Person


# todo: optional
# todo: see typing definition in field
# todo: string as type (order of reference)
# todo: mount only ref root
with Namespace("components") as components:
    with Namespace("schemas", ns=components) as schemas:
        # schemas.mount(Person)
        schemas.mount(People)
        schemas.mount(XPerson)
        schemas.mount(PersonWithNickname)
    assert get_resolver().lookup.lookup(components, "schemas/Person") is not None
    loading.dumpfile(components.as_dict(), format="json")

# schemas = Namespace("schemas")
# schemas.mount(People)

# with Namespace("components") as components:
#     components.mount(schemas)
#     loading.dumpfile(components.as_dict(), format="json")
