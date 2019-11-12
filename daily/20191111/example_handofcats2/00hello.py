from handofcats import as_command


@as_command
def hello(name: str) -> None:
    print(f"hello {name}")
