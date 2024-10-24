import argparse

parser = argparse.ArgumentParser(
    "python-argparse",
    description="description of python-argparse",
    formatter_class=type(
        "_HelpFormatter",
        (argparse.RawTextHelpFormatter, argparse.ArgumentDefaultsHelpFormatter),
        {},
    ),
)
parser.add_argument("--version", required=True, type=str, help="Set version")
parser.add_argument(
    "--no-color", action="store_false", dest="color", help="Disable color"
)
parser.add_argument("positionals", nargs="*", help="Positional arguments", default=[])

parser.print_usage = parser.print_help  # hack to print help on error


# python python-argparse.py --version=1.0.0
# python python-argparse.py --no-color --version=1.0.0
#
# hmm
# python python-argparse.py
# python python-argparse.py --colr=false --versionn=1.0.0 x y z
args = parser.parse_args()
print(args)
