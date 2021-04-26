import typer


def hello(*, name: str = typer.Option("", help="name of greeting")) -> None:
    print(f"hello {name}")


if __name__ == "__main__":
    typer.run(hello)
