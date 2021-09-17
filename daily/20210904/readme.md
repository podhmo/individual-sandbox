## python 型推論でliteral typesが導出されてしまう話

これは面白いなー。

```py
from typing import Iterable, Tuple, TypeVar

K = TypeVar("K")
V = TypeVar("V")

def get_first(source: Iterable[Tuple[K, V]]) -> Tuple[K, V]:
    ...


def main() -> None:
    a = get_first([(1, 2)]) # Tuple[int*, int*]
    b: Tuple[int, int] = get_first([(1, 10)])
    a == b
```

typescriptで似た話

- https://stackoverflow.com/questions/65743826/typescript-generics-constraints-and-literal-types

### misc

mypyだと `int*` で pyrightだと `Literal[1]` とかになるっぽい。

## vscode

toggle pannelでoutputが消せる
toggle sidebar visibilityでサイドバーが消せる