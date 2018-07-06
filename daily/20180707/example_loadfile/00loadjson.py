import json
import sys


def loadfile(f=None):
    if f == None:
        return json.load(sys.stdin)
    with open(f) as rf:
        return json.load(rf)


xs = loadfile("data.json")
print(xs)
