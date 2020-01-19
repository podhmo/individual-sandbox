import argparse
from argparse import _


parser = argparse.ArgumentParser(add_help=False)
parser.add_argument(
    "-h", "--help", action="store_true", help=_("show this help message and exit"),
)

parser.add_argument("--foo")
args, rest = parser.parse_known_args()
print(args, rest)
print("**END**")
