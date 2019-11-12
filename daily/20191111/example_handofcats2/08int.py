from handofcats import as_command


@as_command
def run(x: int, y: int) -> None:
    print(f"{x} + {y} = {x + y}")
