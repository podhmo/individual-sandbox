from __future__ import annotations
import typing as t
import typing_extensions as tx

T = t.TypeVar("T")


class ApplyTarget(tx.Protocol):
    def __apply__(self, evaluator: Evaluator) -> None:
        ...


class GenID:
    def __init__(self, i: int) -> None:
        self.i = i

    def __call__(self) -> str:
        i = self.i
        self.i += 1
        return str(i)


class Evaluator:
    def __init__(self):
        self.genid = GenID(0)

    def eval(self, xs: t.List[ApplyTarget]) -> t.List[ApplyTarget]:
        # load

        # apply
        for x in xs:
            assert isinstance(x, Base), f"{x} is not instance of Base"
            x.__apply__(self)
        return xs

    def load(self) -> t.Dict[str, ApplyTarget]:
        ...


_genid = GenID(0)
_pool: t.Dict[t.Tuple[object, str], "ref"] = {}  # todo: weakref


class Ref(t.Generic[T]):
    def __init__(self, name: str = "????", *, cls=None):
        self.cls = cls or self.__class__
        self.name = name

    def __set_name__(self, cls: t.Any, name: str) -> None:
        self.cls = cls
        self.name = name

    def __repr__(self):
        return f"Ref({self.cls.__name__}.{self.name})"

    def __get__(  # noqa:F811
        self, ob: t.Optional[t.Any], typ: t.Optional[t.Type[t.Any]] = None
    ) -> t.Union[T, ref]:
        if ob is None:
            return self

        k = (ob, self.name)
        v = _pool.get(k)
        if v is None:
            v = _pool[k] = ref(ob, self.name)
        if v.value is None:
            return v
        return v.value

    def __set__(self, ob: t.Any, value: T) -> None:
        k = (ob, self.name)
        if isinstance(value, ref):
            _pool[k] = value
            return

        v = _pool.get(k)
        if v is None:
            v = _pool[k] = ref(ob, self.name)
        v.value = value


class ref:
    def __init__(self, ob: object, name: str):
        self.ob = ob
        self.name = name
        self.id = _genid()
        self.value = None

    def __repr__(self):
        return f"ref({self.ob.__class__.__name__}.{self.name}#{self.id})"

    def resolve(self, ev: Evaluator, ob: ApplyTarget, name: str) -> T:
        if self.value is None:
            self.value = ev.genid()  # genid?
        return self.value


class merge_ref:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def resolve(self, ev: Evaluator, ob: ApplyTarget, name: str) -> T:
        x = self.x
        if hasattr(x, "resolve"):
            x = x.resolve(ev, ob, name)
        y = self.y
        if hasattr(y, "resolve"):
            y = y.resolve(ev, ob, name)
        return x + y


class format_ref:
    def __init__(self, fmt, *args, **kwargs):
        self.fmt = fmt
        self.args = args
        self.kwargs = kwargs

    def resolve(self, ev: Evaluator, ob: ApplyTarget, name: str) -> T:
        fmt = self.fmt
        args = [
            x.resolve(ev, ob, name) if hasattr(x, "resolve") else x for x in self.args
        ]
        kwargs = {
            k: x.resolve(ev, ob, name) if hasattr(x, "resolve") else x
            for k, x in self.kwargs.items()
        }
        return fmt.format(*args, **kwargs)


class Base:
    _fulfilled: bool = False

    def __init_subclass__(cls):
        hints = t.get_type_hints(cls)
        fields = []
        for k, v in hints.items():
            if k.startswith("_"):
                continue
            setattr(cls, k, Ref(k, cls=cls))
            fields.append(k)
        cls._fields = fields

        if "__init__" not in cls.__dict__:
            args = ", ".join([f"{name}=None" for name in fields])
            lines = [f"def __init__(self, {args}):"]
            for name in fields:
                lines.append(f"    if {name} is not None:")
                lines.append(f"        self.{name} = {name}")
            D = {}
            code = "\n".join(lines)
            exec(code, D)
            cls.__init__ = D["__init__"]
        super().__init_subclass__()

    def __repr__(self):
        prefix = "" if self._fulfilled else "?"
        args = ", ".join([f"{k}={getattr(self, k)!r}" for k in self.__class__._fields])
        return f"{prefix}{self.__class__.__name__}({args})"

    def __apply__(self, ev: Evaluator) -> None:
        if self._fulfilled:
            return

        self._fulfilled = True  # xxx
        for name in self.__class__._fields:
            v = getattr(self, name)
            if hasattr(v, "resolve"):
                v = v.resolve(ev, self, name)
                setattr(self, name, v)


class A(Base):
    name: str
    id: str


class B(Base):
    name: str
    a_id: str
    id: str


class C(A):
    pass


def run():
    a = A()
    b = B(name=a.name, a_id=a.id)
    c = C()
    return tuple(locals().values())


print(Evaluator().eval(run()))
# for (ob, k), ref in _pool.items():
#     print(id(ob), k, "->", ref, ref.value)
