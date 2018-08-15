import ascommand


@ascommand.as_command
def run(*, x: int, y: int):
    print(f"{x} + {y} = {x + y}")
