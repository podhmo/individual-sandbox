import typing as t
import argparse
from handofcats.injector import Injector


def main(argv: t.Optional[t.List[str]] = None) -> None:
    from argparse import _

    # 1st parser
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument(
        "-h", "--help", action="store_true", help=_("show this help message and exit"),
    )
    parser.add_argument("--fn", required=False, choices=[x.__name__ for x in [f, g]])
    args, rest_args = parser.parse_known_args(argv)

    if args.fn is None and args.help:
        parser.print_help()
        parser.exit()

    if args.help:
        rest_args.append("-h")
    fn = globals()[args.fn]

    # 2nd parser
    parser = argparse.ArgumentParser()
    Injector(fn).inject(parser, help_default=None)
    args = parser.parse_args(rest_args)
    params = vars(args).copy()

    print(fn(**params))


def f(*, name: str, y: str, z: t.Optional[str] = None, verbose: bool = True):
    r = (name, y, z)
    return r, [type(x) for x in r]


def g(*, name: str, i: int, z: t.Optional[int] = 100, verbose: bool = True):
    r = (name, i, z)
    return r, [type(x) for x in r]


if __name__ == "__main__":
    main()
