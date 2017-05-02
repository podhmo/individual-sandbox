import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-n", type=int, action="append")
parser.add_argument("--fn", choices=[max, min, sum], action="store_const", required=True)
args = parser.parse_args()
args.fn(args.n)
