import typing_extensions as tx
from monogusa import default_component
from monogusa.dependencies import resolve_args


@tx.runtime_checkable
class P(tx.Protocol):
    def foo(self) -> str:
        ...


class I:
    def foo(self) -> str:
        return "I"


@default_component
def foo() -> P:
    return I()


def use(p: P) -> None:
    print(p.foo())


use(*resolve_args(use))
