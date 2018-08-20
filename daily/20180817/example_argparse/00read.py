import sys
import argparse
import json

parser = argparse.ArgumentParser()
parser.add_argument("--input-file")
args = parser.parse_args()

if args.input_file is None:
    data = json.load(sys.stdin)
else:
    with open(args.input_file) as rf:
        data = json.load(rf)
print(data)
