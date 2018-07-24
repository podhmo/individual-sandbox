from typing import Optional


class A:
    def __init__(self, x: str) -> None:
        self.x = x


class B:
    x: str = "foo"

    def __init__(self, x: Optional[str] = None) -> None:
        if x is not None:
            self.x = x


print(A("foo").__dict__)  # {'x': 'foo'}
print(B("foo").__dict__)  # {'x': 'foo'}
print(B().__dict__)  # {}
