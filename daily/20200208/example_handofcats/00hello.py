from handofcats import as_command


@as_command
def hello(*, name="world"):
    print(f"hello, {name}")
