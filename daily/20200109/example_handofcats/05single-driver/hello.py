import typing as t
from handofcats import as_command


@as_command
def hello(name: str, *, nickname: t.Optional[str]):
    print(name, nickname)
