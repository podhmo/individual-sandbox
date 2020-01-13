import typing as t


def hello(name: str, nick_name: t.Optional[str] = None) -> None:
    print(f"hello {name}")
    if nick_name is not None:
        print(f"	{nick_name}?")
