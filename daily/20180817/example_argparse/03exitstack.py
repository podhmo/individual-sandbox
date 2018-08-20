import argparse
import contextlib
import json

parser = argparse.ArgumentParser()
parser.add_argument("--input-file", type=argparse.FileType("r"), default="-")
args = parser.parse_args()
with contextlib.ExitStack() as s:
    if hasattr(args.input_file, "name"):
        s.enter_context(args.input_file)  # or s.callback(args.input_file.close)
    data = json.load(args.input_file)
print(data)
