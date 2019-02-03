import sys
import argparse
from dictknife import loading
from dictknife.cliutils.extraaguments import ExtraArgumentsParsers
from importlib import import_module

formats = loading.get_formats()

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--format", choices=formats, default="json")

ex_parsers = ExtraArgumentsParsers(parser, "--format")
for f in formats:
    if f == "markdown":  # xxx:
        continue

    m = import_module(f"dictknife.loading.{f}")
    ex_parser = ex_parsers.add_parser(f)
    setup = getattr(m, "setup_parser", None)
    if setup is None:
        print(f"{m.__name__} doesn't have setup_parser() function", file=sys.stderr)
        continue
    setup(ex_parser)

args, rest = parser.parse_known_args()
extra, _ = ex_parsers.parse_args(args.format, rest)
print(args, extra)

L = [
    {
        "name": "foo",
        "age": 20
    },
    {
        "name": "bar",
        "age": 21,
        "nickname": "B"
    },
    {
        "name": "boo"
    },
]
loading.dumpfile(L, **vars(args), extra=vars(extra))
