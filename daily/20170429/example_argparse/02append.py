import argparse


parser = argparse.ArgumentParser()
parser.add_argument("--module", action="append", type=set)
args = parser.parse_args()
print(args)
