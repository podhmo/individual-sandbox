# from: https://pod.hatenablog.com/entry/2018/08/13/231332
import typing as t
import inspect
import itertools
from functools import partial
from collections import ChainMap
import argparse


def find_original_with_arguments(fn):
    args = ()
    kwargs = {}
    if isinstance(fn, partial):
        args = fn.args
        kwargs = fn.keywords
        fn = fn.func
    if inspect.isclass(fn):
        fn = fn.__init__
    return fn, args, kwargs


def _make_parser(*args, **kwargs):
    parser = argparse.ArgumentParser(*args, **kwargs)
    parser.print_usage = parser.print_help
    return parser


def main(argv: t.Optional[t.List[str]] = None) -> None:
    parser = _make_parser(add_help=False)
    parser.add_argument("--verbose", action="store_true")
    parser.add_argument("--name", required=False)
    parser.add_argument("--fn", required=True, choices=[x.__name__ for x in [f, g]])
    args, rest_args = parser.parse_known_args(argv)

    cli_kwargs = vars(args).copy()
    fn = globals()[cli_kwargs.pop("fn")]

    original, fn_args, default_kwargs = find_original_with_arguments(fn)
    spec = inspect.getfullargspec(original)
    second_parser = _make_parser(fn.__name__)
    seen = set()
    for name, default, required in itertools.chain(
        ((k, v, False) for k, v in spec.kwonlydefaults.items() or ()),
        ((k, None, True) for k in spec.kwonlyargs or ()),
    ):
        if name in seen:
            continue
        seen.add(name)

        clitype = None
        typ = spec.annotations.get(name)
        if typ is not None:
            if typ in (int, float):
                clitype = typ  # todo: supporting only int,

        default = default_kwargs.get(name) or default
        help_message = None
        if default is not None:
            help_message = f"(default={default_kwargs.get(name) or default!r})"

        if name in cli_kwargs:
            default = cli_kwargs[name]
            required = False

        second_parser.add_argument(
            f"--{name.replace('_', '-')}",
            type=clitype,
            required=required,
            default=default,
            help=help_message,
        )

    second_args = second_parser.parse_args(rest_args)
    return original(*fn_args, **ChainMap(default_kwargs, cli_kwargs, vars(second_args)))


def f(*, name: str, y: str, z: t.Optional[str] = None, verbose: bool = True):
    r = (name, y, z)
    return r, [type(x) for x in r]


def g(*, name: str, i: int, z: t.Optional[int] = 100, verbose: bool = True):
    r = (name, i, z)
    return r, [type(x) for x in r]


if __name__ == "__main__":
    main()
