import argparse

parser = argparse.ArgumentParser(
    prog="app", formatter_class=argparse.ArgumentDefaultsHelpFormatter
)

# not noneじゃないとだめっぽい
parser.add_argument("--debug", action="store_true", help="-")
parser.add_argument("file", default="foo", nargs="?", help="-")
parser.print_help()
