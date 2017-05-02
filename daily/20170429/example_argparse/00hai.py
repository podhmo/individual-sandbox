import argparse

parser = argparse.ArgumentParser("hai")
parser.add_argument("--file", required=True)
args = parser.parse_args()

print(args)
