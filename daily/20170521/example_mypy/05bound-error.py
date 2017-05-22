from typing import TypeVar

T = TypeVar("T", bound="typing.Union(int, str)")
n : T = 10
print(n)
