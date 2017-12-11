import yaml
import sys


with open("data.json", "r") as rf:
    d = yaml.load(rf)

yaml.dump(d, sys.stdout, default_flow_style=False)
