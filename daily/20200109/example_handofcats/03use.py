from handofcats import as_command


@as_command
def hello(*, name: str="world") -> None:
    print(f"hello {name}")
