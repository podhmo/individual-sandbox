import argparse
import json

parser = argparse.ArgumentParser()
parser.add_argument("--output-file", type=argparse.FileType("w"), default="-")
args = parser.parse_args()

data = {"msg": "hello"}
json.dump(data, args.output_file, indent=2)
