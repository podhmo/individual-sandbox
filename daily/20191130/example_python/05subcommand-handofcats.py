import argparse
from handofcats.injector import Injector


def add(*, x: int, y: int) -> int:
    return x + y


def hello(*, name: str = "world") -> None:
    print(f"hello {name}")


parser = argparse.ArgumentParser()
sparsers = parser.add_subparsers(required=True, title="subcommands")


Injector(add).inject(sparsers.add_parser("add"))
Injector(hello).inject(sparsers.add_parser("hello"))

# parser.parse_args(["add", "-h"])
# parser.parse_args(["hello", "-h"])
parser.parse_args(["-h"])
