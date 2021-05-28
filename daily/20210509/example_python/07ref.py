from __future__ import annotations
import typing as t
import typing_extensions as tx

T = t.TypeVar("T")


class Evaluator(tx.Protocol):
    def load(self) -> t.Dict[str, EvalTarget]:
        ...

    def eval(self, ob: EvalTarget) -> None:
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


class Ref(t.Generic[T]):
    def __init__(self, name):
        self.name: str = name
        self.value: t.Union[T, None] = _dummy

    def __repr__(self):
        return f"ref('{self.name}')"

    def resolve(self, ev: Evaluator) -> T:
        if self.value is _dummy:
            self.value = ev.genid()
        return self.value


class A:
    name: str = Ref("name")
    id: str = Ref("id")
    _fulfilled: bool = False

    def __init__(self, *, name: t.Union[str, Ref[str]]) -> None:
        self.name = name

    def __repr__(self):
        return f"A(name={self.name!r}, id={self.id!r})"

    def __eval__(self, ev: Evaluator):
        if self._fulfilled:
            return

        self._fulfilled = True
        if isinstance(self.name, Ref):
            self.name = self.name.resolve(ev)
        if isinstance(self.id, Ref):
            self.id = self.id.resolve(ev)


class B:
    name: str = Ref("name")
    a_id: t.Union[str, Ref] = Ref("a_id")
    id: str = Ref("id")
    _fulfilled: bool = False

    def __init__(
        self, *, name: t.Union[str, Ref[str]], a_id: t.Union[str, Ref[str]]
    ) -> None:
        self.name = name
        self.a_id: t.Union[str, Ref[str]] = a_id

    def __repr__(self):
        return f"B(name={self.name!r}, a_id={self.a_id!r}, id={self.id!r})"

    def __eval__(self, ev: Evaluator):
        if self._fulfilled:
            return

        self._fulfilled = True
        if isinstance(self.name, Ref):
            self.name = self.name.resolve(ev)
        if isinstance(self.a_id, Ref):
            self.a_id = self.a_id.resolve(ev)
        if isinstance(self.id, Ref):
            self.id = self.id.resolve(ev)


def run(name):
    a = A(name=name)
    b = B(name=a.name, a_id=a.id)
    return (a, b)


ref = Ref("name")
xs = run(ref)
ev = MyEvaluator()
ref.value = "xxx"
for x in xs:
    ev.eval(x)
print(xs)
