import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-n", type=int, help="number of something")
parser.add_argument("--name", help="name of something")
args = parser.parse_args()
print(args.n, args.name)
