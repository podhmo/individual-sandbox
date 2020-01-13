# https://docs.python.org/3/library/argparse.html#fromfile-prefix-chars
import argparse
import json

parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
parser.add_argument("--cmd", dest="commands", action="append")
args = parser.parse_args(["@commands.txt"])
print(args)

parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
parser.add_argument("data", nargs="+")
args = parser.parse_args(["@commands.txt"])
print(args)
