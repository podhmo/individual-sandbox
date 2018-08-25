import sys
import argparse
from typing import Callable

# types
ActionFunction = Callable[[argparse.Namespace], None]


def action(subparser: argparse.ArgumentParser) -> Callable[[ActionFunction], ActionFunction]:
    """Decorator to tie an action function to a subparser."""

    def register(func: ActionFunction) -> ActionFunction:
        subparser.set_defaults(action=func)
        return func

    return register


parser = argparse.ArgumentParser()
parser.add_argument("--verbose", action="store_true")
parser.set_defaults(action=None)
subparsers = parser.add_subparsers()

p = hello_parser = subparsers.add_parser("hello")
p.add_argument("name")
del p


@action(hello_parser)
def hello(args: argparse.Namespace):
    print("hello", args.name, args.verbose)


p = show_parser = subparsers.add_parser("show")
p.add_argument("n", type=int)
del p


@action(show_parser)
def show(args: argparse.Namespace):
    print("show", args.n, args.verbose)


def main() -> None:
    args = parser.parse_args()

    if not args.action:
        parser.print_usage()
        sys.exit(1)
    else:
        args.action(args)


if __name__ == "__main__":
    main()
