import json
import sys

d = {"name": "何か"}

json.dump(d, sys.stdout)
json.dump(d, sys.stdout, ensure_ascii=False)
