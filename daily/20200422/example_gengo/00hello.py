from handofcats import as_command


def hello(name: str) -> str:
    return f"Hello {name}"


@as_command
def run(*, name: str) -> None:
    print(hello(name))
