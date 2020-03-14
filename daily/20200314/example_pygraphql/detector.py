import typing as t
import itertools
from functools import partial
import dataclasses
from collections import defaultdict

# todo: hetero list
# todo: many=True

uid = str
Signature = t.Any
TypeInfo = t.Union["Object", "Container", "Primitive"]
Path = t.List[str]
JSONType = t.Union[int, float, str, bool, t.List["JSONType"], t.Dict[str, "JSONType"]]


class Object:
    def __init__(
        self, sig: t.Any, *, path: Path, props: t.Dict, raw: t.Dict[t.Any, t.Any]
    ) -> None:
        self.sig = sig
        self.path = path
        self.props = props
        self._raw = raw
        self._others: t.List[t.Tuple[Path], t.Dict[t.Any, t.Any]] = []

    def clone(self) -> "Object":
        new = self.__class__(self.sig, path=self.path, props=self.props, raw=self._raw)
        new._others = self._others
        return new

    @property
    def size(self) -> int:
        return len(self.props)

    def __repr__(self):
        return f"<Object size={self.size} path={self.path!r}>"


class Container:
    def __init__(
        self,
        sig: t.Any,
        *,
        path: Path,
        base: t.Type[t.Any],
        item: TypeInfo,
        raw: t.Dict[t.Any, t.Any],
    ) -> None:
        self.sig = sig
        self.path = path
        self.base = base
        self.item = item
        self._raw = raw
        self._others: t.List[t.Tuple[Path], t.Dict[t.Any, t.Any]] = []

    @property
    def size(self) -> int:
        if hasattr(self._raw, "__len__"):
            return len(self._raw)
        return self.item.size

    def clone(self) -> "Container":
        new = self.__class__(
            self.sig, path=self.path, base=self.base, item=self.item, raw=self._raw
        )
        new._others = self._others
        return new

    def __repr__(self):
        return f"<Container base={self.base} size={self.size} path={self.path!r}>"


ListC = partial(Container, base=t.List)
OptionalC = partial(Container, base=t.Optional)


class Primitive:
    def __init__(
        self,
        sig: t.Any,
        *,
        path: Path,
        type_: t.Type[t.Any],
        raw: t.Dict[t.Any, t.Any],
    ) -> None:
        self.sig = sig
        self.path = path
        self.type = type_
        self._raw = raw
        self._others: t.List[t.Tuple[Path], t.Dict[t.Any, t.Any]] = []

    @property
    def size(self) -> int:
        return 0

    def clone(self) -> "Primitive":
        new = self.__class__(self.sig, path=self.path, type_=self.type, raw=self._raw)
        new._others = self._others
        return new

    def __repr__(self):
        return f"<Primitive type={self.type!r} path={self.path!r}>"


SENTINEL = object()
ZERO = Object(tuple(), path=":zero:", props={}, raw={})


class Result:
    def __init__(self):
        self.registry: t.Dict[uid, TypeInfo] = {}
        self._uid_map: t.Dict[Signature, uid] = defaultdict(lambda: len(self._uid_map))
        self._dead_set: t.Set[Signature] = set()

    def __repr__(self):
        return f"<Result size={len(self.history)}>"

    def add(self, uid: uid, info: TypeInfo) -> TypeInfo:
        self.registry[uid] = info  # ordered
        return info

    @property
    def history(self) -> t.List[TypeInfo]:
        return [
            info for info in self.registry.values() if info.sig not in self._dead_set
        ]

    def __contains__(self, uid: uid):
        return uid in self.registry

    def get(self, uid: uid, *, default):
        return self.registry.get(uid, default)


class Detector:
    def __init__(self, config: "t.Optional[Config]" = None) -> None:
        self.config = config or Config()

    # todo: many
    def detect(self, d: JSONType, *, path: Path, result: Result) -> TypeInfo:
        if isinstance(d, dict):
            return self.detect_dict(d, path=path, result=result)
        elif isinstance(d, (list, tuple)):
            return self.detect_list(d, path=path, result=result)
        else:
            return self.detect_primitive(d, path=path, result=result)

    # def detect_many(self, d: JSONType, *, path: Path, result: Result) -> TypeInfo:
    #     if isinstance(d, dict):
    #         return self.detect_dict_many(d, path=path, result=result)
    #     elif isinstance(d, (list, tuple)):
    #         return self.detect_list_many(d, path=path, result=result)
    #     else:
    #         return self.detect_primitive_many(d, path=path, result=result)

    def detect_dict_many(
        self,
        xs: t.Collection[t.Dict[JSONType, JSONType]],
        *,
        path: Path,
        result: Result,
    ) -> TypeInfo:
        seen: t.Dict[int, TypeInfo] = {}

        modified = False
        ma = ZERO
        union_members = []

        # TODO: update signature?
        # TODO: union
        if len(xs) > 0:
            ma = self.detect_dict(xs[0], path=path, result=result)
            seen[id(ma)] = ma

        for x in self.config.iterate_candidates(xs, from_=1):
            info = self.detect_dict(x, path=path, result=result)

            if id(info) in seen:
                continue
            seen[id(info)] = info

            if not modified:
                modified = True
                ma = ma.clone()

            if not isinstance(info, Object):
                # FIXME: union support
                union_members.append(info)
                continue
                # raise RuntimeError(f"{type(info)} is not Object")

            for k, val in info.props.items():
                ma_val = ma.props.get(k)

                if ma_val is None:
                    ma.props[k] = OptionalC(None, path=val.path, item=val, raw=val._raw)
                elif getattr(val, "base", None) is t.Optional:  # OptionalC
                    if getattr(ma_val, "base", None) is None:
                        assert ma_val == val.item
                        ma.props[k] = val
            for k, ma_val in ma.props.items():
                val = info.props.get(k)
                if val is None:
                    if getattr(ma_val, "base", None) is None:
                        ma.props[k] = OptionalC(
                            None, path=ma_val.path, item=ma_val, raw=ma_val._raw
                        )

        if modified:
            uid = result._uid_map[ma.sig]
            for other in seen.values():
                if other.sig != ma.sig and other.sig not in result._dead_set:
                    result._dead_set.add(other.sig)
            result.add(uid, ma)

        # FIXME: union support
        if union_members:
            import sys

            print(f"ignored: {union_members}", file=sys.stderr)
        #     for x in union_members:
        #         print("@", id(x), x)
        return ma

    def detect_dict(
        self, d: t.Dict[JSONType, JSONType], *, path: Path, result: Result
    ) -> TypeInfo:
        props = {}
        sigs = []

        try:
            pairs = d.items()
        except AttributeError:
            return self.detect(d, path=path, result=result)

        for k, v in pairs:
            path.append(k)
            subinfo = props[k] = self.detect(v, path=path, result=result)
            sigs.append((k, subinfo.sig))
            path.pop()

        sig = frozenset(sigs)
        uid = result._uid_map[sig]
        info = result.get(uid, default=SENTINEL)

        if info is not SENTINEL:
            info._others.append((path[:], d))
            return result.registry[uid]

        return result.add(uid, Object(sig, path=path[:], raw=d, props=props))

    def detect_list(
        self, d: t.List[JSONType], *, path: Path, result: Result
    ) -> TypeInfo:
        path.append(":item:")
        inner_info = self.detect_dict_many(d, path=path, result=result)
        path.pop()
        return result.add(
            uid, ListC((list, inner_info.sig), item=inner_info, path=path[:], raw=d)
        )

    def detect_primitive(self, d: t.Any, *, path: Path, result: Result) -> TypeInfo:
        sig = type(d)
        uid = result._uid_map[sig]
        if uid in result:
            cached = result.registry[uid]
            if cached.path == path:
                return cached
            else:
                # todo: performance improvement
                new = cached.clone()
                new._others.append(d)
                new.path = path[:]
                return new

        return result.add(uid, Primitive(sig, path=path[:], raw=d, type_=type(d)))


@dataclasses.dataclass
class Config:
    n: int = 100

    def iterate_candidates(
        self, xs: t.List[t.Any], *, from_: int = 0
    ) -> t.Iterator[t.Any]:
        if self.n >= len(xs):
            return iter(xs)
        return itertools.chain(xs[from_ : self.n], xs[-self.n :])


def detect(d: JSONType, *, detector=Detector()):
    path: Path = []
    result = Result()
    detector.detect(d, path=path, result=result)
    return result


def show(d):
    print("-")
    r = detect(d)
    print(r)

    for info in r.history:
        print(" ", info)


def generate_annotations(
    result: Result, *, use_fullname: bool = False, toplevel_name: str = "Toplevel"
) -> t.Dict[str, t.Dict[str, t.Any]]:
    from prestring.utils import NameStore
    from inflection import pluralize, singularize, camelize

    ns = NameStore()
    r = {}

    for info in result.history:
        if isinstance(info, Object):
            if use_fullname:
                path = info.path[:]
                if path and path[-1].startswith(":"):
                    path = path[:-1]
                if path and pluralize(path[-1]) == path[-1]:
                    path[-1] = singularize(path[-1])
                r["/".join(info.path)] = {
                    "before": {"name": "_".join(camelize(x) for x in path)}
                }
            else:
                name = info.path[-1] if info.path else toplevel_name
                if name.startswith(":"):
                    name = info.path[-2]
                if pluralize(name) == name:
                    name = singularize(name)
                ns[info] = name
                r["/".join(info.path)] = {"before": {"name": camelize(ns[info])}}
    return r
