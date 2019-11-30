from handofcats.parsers.expose import create_parser
from handofcats.injector import Injector


def hello(*, name: str) -> None:
    print(f"hello {name}")


parser = create_parser(hello)
Injector(hello).inject(parser)
print(parser)
