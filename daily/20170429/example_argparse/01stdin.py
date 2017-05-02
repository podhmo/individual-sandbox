import sys
import argparse


parser = argparse.ArgumentParser("hai")
parser.add_argument("--file", nargs="?", type=argparse.FileType("r"), default=sys.stdin)
args = parser.parse_args()

print(args.file.read())
