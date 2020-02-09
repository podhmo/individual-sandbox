from handofcats import as_command


@as_command
def hello(*, name="world") -> str:
    return f"hello, {name}"
