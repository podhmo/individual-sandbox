import typing_extensions as tx


C = tx.Literal[list, dict]  # error


def run(x: C) -> None:
    print(x)


def main() -> None:
    run(list)
    run(dict)
    run(tuple)
