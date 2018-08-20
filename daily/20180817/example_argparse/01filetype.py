import argparse
import json

parser = argparse.ArgumentParser()
parser.add_argument("--input-file", type=argparse.FileType("r"), default="-")
args = parser.parse_args()
data = json.load(args.input_file)
print(data)
