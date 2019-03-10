import typing as t


class A:
    def __init__(self, *, stack: t.Optional[t.List["A"]] = None) -> None:
        self.stack = stack or []

    def new_child(self) -> "A":
        stack = self.stack[:]
        stack.append(self)
        return A(stack=stack)


print(A().new_child().new_child().stack)
