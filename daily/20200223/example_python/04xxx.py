import typing_extensions as tx


class P(tx.Protocol):
    def foo(self) -> str:
        ...


class M:
    def bar(self: P) -> None:
        print(self.foo(), self.foo())

        # error: "P" has no attribute "boo"
        print(self.boo())

    def boo(self) -> str:
        return "boo"
