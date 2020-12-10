import json
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--config", required=True)
args = parser.parse_args()
with open(args.config) as rf:
    config = json.load(rf)
print(config["replica"])
