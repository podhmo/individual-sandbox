from __future__ import annotations
import typing as t
import typing_extensions as tx

T = t.TypeVar("T")


class Evaluator(tx.Protocol):
    def load(self) -> t.Dict[str, EvalTarget]:
        ...

    def eval(self, ob: EvalTarget) -> t.Any:
        ...

    def genid(self) -> str:
        ...


class EvalTarget(tx.Protocol):
    def __eval__(self, evaluator: Evaluator) -> None:
        ...


class GenID:
    def __init__(self, i: int) -> None:
        self.i = i

    def __call__(self) -> str:
        i = self.i
        self.i += 1
        return str(i)


class MyEvaluator:
    def __init__(self):
        self.genid = GenID(0)

    def eval(self, ob: EvalTarget) -> None:
        ob.__eval__(self)

    def load(self) -> t.Dict[str, EvalTarget]:
        ...


_dummy = None
_genid = GenID(0)
_pool: t.Dict[t.Tuple[object, str], "ref"] = {}  # todo: weakref


class Ref(t.Generic[T]):
    def __init__(self):
        self.name = "????"

    def __set_name__(self, ob: t.Any, name: str) -> None:
        self.name = name

    def __get__(  # noqa:F811
        self, ob: t.Optional[t.Any], typ: t.Optional[t.Type[t.Any]] = None
    ) -> t.Union[T, ref]:
        if ob is None:
            return self

        k = (ob, self.name)
        v = _pool.get(k)
        if v is None:
            v = _pool[k] = ref(ob, self.name)
        if v.value is _dummy:
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

    def resolve(self, ev: Evaluator, ob: EvalTarget, name: str) -> T:
        if self.value is _dummy:
            self.value = ev.genid()  # genid?
        setattr(ob, name, self.value)


class A:
    name: t.Union[str, ref] = Ref()
    id: t.Union[str, ref] = Ref()

    _fulfilled: bool = False

    def __init__(
        self,
        *,
        name: t.Union[str, Ref[str], None] = None,
        id: t.Union[str, Ref[str], None] = None,
    ) -> None:
        if name is not None:
            self.name = name
        if id is not None:
            self.id: t.Union[str, Ref[str]] = id

    def __repr__(self):
        prefix = "" if self._fulfilled else "?"
        return f"{prefix}A(name={self.name!r}, id={self.id!r})"

    def __eval__(self, ev: Evaluator) -> None:
        if self._fulfilled:
            return

        self._fulfilled = True  # xxx
        for name in ["name", "id"]:
            v = getattr(self, name)
            if hasattr(v, "resolve"):
                v = v.resolve(ev, self, name)


class B:
    name: t.Union[str, ref] = Ref()
    a_id: t.Union[str, ref] = Ref()
    id: t.Union[str, ref] = Ref()

    _fulfilled: bool = False

    def __init__(
        self,
        *,
        name: t.Union[str, Ref[str], None] = None,
        id: t.Union[str, Ref[str], None] = None,
        a_id: t.Union[str, Ref[str], None] = None,
    ) -> None:
        if name is not None:
            self.name = name
        if a_id is not None:
            self.a_id: t.Union[str, Ref[str]] = a_id
        if id is not None:
            self.id: t.Union[str, Ref[str]] = id

    def __repr__(self):
        prefix = "" if self._fulfilled else "?"
        return f"{prefix}B(name={self.name!r}, a_id={self.a_id!r}, id={self.id!r})"

    def __eval__(self, ev: Evaluator) -> None:
        if self._fulfilled:
            return

        self._fulfilled = True  # xxx
        for name in ["name", "a_id", "id"]:
            v = getattr(self, name)
            if hasattr(v, "resolve"):
                v.resolve(ev, self, name)


def run():
    a = A()
    b = B(name=a.name, a_id=a.id)
    return (a, b)


xs = run()
ev = MyEvaluator()
a, b = xs
print(a, b)
a.name = "foo"
print(a, b)
for x in reversed(xs):
    ev.eval(x)
print(xs)
