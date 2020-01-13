import typing as t
from handofcats import as_command


@as_command
def hello(name: str, nick_name: t.Optional[str] = None) -> None:
    print(f"hello {name}")
    if nick_name is not None:
        print(f"	{nick_name}?")
