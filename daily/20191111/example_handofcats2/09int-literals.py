import typing as t
import typing_extensions as tx
from handofcats import as_command


@as_command
def run(*, alpha: tx.Literal[1, 2, 3, 4, 5, 6, 7, 8, 9], ps: t.List[int]) -> None:
    print([alpha * p for p in ps])
