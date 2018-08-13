## python argparse 相互排他


## python wsgi appの型

```python
from typing import (
    Dict,
    Callable,
    Text,
    Any,
    Iterable,
    Union,
    List,
    Tuple,
    Optional,
    Type,
    TracebackType,
)

_exc_info = Tuple[Optional[Type[BaseException]], Optional[BaseException], Optional[TracebackType]]

WSGIEnvironment = Dict[Text, Any]

StartResponse = Union[Callable[[Text, List[Tuple[Text, Text]]], Callable[[bytes], None]],
                      Callable[[Text, List[Tuple[Text, Text]], _exc_info], Callable[[bytes], None]]]

WSGIApplication = Callable[[WSGIEnvironment, StartResponse], Iterable[bytes]]
```

:warning: Unionはおかしい。

- protocolにする
- mypy_extensions.VarArg使う

## python wsgirefとかの方

- https://github.com/python/typeshed/tree/master/stdlib/2and3/wsgiref

