import argparse
from handofcats.driver import Driver, Config


def hello(name: str, *, age: int = 0):
    """hello, this is tiny hello command"""
    pass


parser, middlewares = Driver(hello).setup_parser()
print(parser.format_help())

print("----------------------------------------")
parser, middlewares = Driver(hello).setup_parser(config=Config(ignore_arguments=True))
print(parser.format_help())
