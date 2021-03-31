import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--file", action="append", dest="files", required=True)
# print(parser.parse_args(["--file", "x", "--file", "y"]))
print(parser.parse_known_args())
print(parser.parse_args())
