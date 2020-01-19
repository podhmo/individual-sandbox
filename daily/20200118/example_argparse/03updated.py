import typing as t
import argparse
from handofcats.injector import Injector


def main(argv: t.Optional[t.List[str]] = None) -> None:
    def _make_parser(*args, **kwargs):
        parser = argparse.ArgumentParser(*args, **kwargs)
        parser.print_usage = parser.print_help
        return parser

    # 1st parser
    parser = _make_parser(add_help=False)
    parser.add_argument("--fn", required=True, choices=[x.__name__ for x in [f, g]])
    args, rest_args = parser.parse_known_args(argv)

    fn = globals()[args.fn]

    # 2nd parser
    parser = _make_parser()
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
