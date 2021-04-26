import typer


def run(*, name: str, verbose:bool, force: bool=False) -> None:
    print(name, force)


if __name__ == "__main__":
    typer.run(run)
