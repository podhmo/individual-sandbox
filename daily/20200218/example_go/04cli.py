from handofcats import as_command, Config


@as_command(config=Config(ignore_expose=True, ignore_logging=True))
def run(*, name: str = "", n: int = 0) -> None:
    print(n, name)
