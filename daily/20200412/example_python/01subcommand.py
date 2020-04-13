from handofcats import as_subcommand


@as_subcommand
def hello(*, name: str, age: int = 0) -> None:
    """HelloCommand"""
    print(f"Hello {name}!!")


@as_subcommand
def byebye(*, name: str) -> None:
    """ByebyeCommand"""
    print(f"Byebye {name}!!")


as_subcommand.run()
