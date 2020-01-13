import typing as t
from handofcats import as_subcommand


@as_subcommand
def hello(name: str, nick_name: t.Optional[str] = None) -> None:
    print(f"hello {name}")
    if nick_name is not None:
        print(f"	{nick_name}?")


@as_subcommand
def byebye(names: t.List[str]) -> None:
    print(f"byebye {', '.join(names)}")


as_subcommand.run()
