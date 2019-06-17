import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--foo", dest="bar", action="store_false")
args = parser.parse_args()
print(args)
