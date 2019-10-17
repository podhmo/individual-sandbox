import typing as t
import typing_extensions as tx
import typing_inspect

print(typing_inspect.get_args(t.Optional[tx.Literal["A", "B"]]))
# (typing_extensions.Literal['A', 'B'], <class 'NoneType'>)
