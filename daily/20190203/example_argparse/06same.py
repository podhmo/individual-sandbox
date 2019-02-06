import argparse

parser = argparse.ArgumentParser()
# duplicated
parser.add_argument("--xyz")
parser.add_argument("--xyz")
print(parser.parse_args(["--xyz"]))
# argparse.ArgumentError: argument --xyz: conflicting option string: --xyz
