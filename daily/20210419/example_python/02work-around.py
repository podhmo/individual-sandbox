import typer


def run(
    name: str = typer.Option(...),
    *,
    verbose: bool = typer.Option(is_flag=True, default=True),
    force: bool = False
) -> None:
    print(name, force)


if __name__ == "__main__":
    typer.run(run)
