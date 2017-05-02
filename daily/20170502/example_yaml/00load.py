import yaml
import sys

data = yaml.load(sys.stdin)
yaml.dump(data, sys.stdout)
