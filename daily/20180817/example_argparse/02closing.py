import argparse
import contextlib
import json

parser = argparse.ArgumentParser()
parser.add_argument("--input-file", type=argparse.FileType("r"), default="-")
args = parser.parse_args()
with contextlib.closing(args.input_file) as rf:
    data = json.load(rf)
print(data)
