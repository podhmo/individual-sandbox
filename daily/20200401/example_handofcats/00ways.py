import typing as t
from handofcats import as_subcommand


@as_subcommand
def foo(*, name: str, age: int, nickname: t.Optional[str] = None):
    pass


@as_subcommand
def bar():
    pass


as_subcommand.run()
