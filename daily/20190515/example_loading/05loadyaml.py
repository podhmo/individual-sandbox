import sys
import yaml
import pprint

mem = {}


def construct_dict(loader, node):
    r = dict(loader.construct_pairs(node))
    mem[id(r)] = node
    return r


yaml.add_constructor("tag:yaml.org,2002:map", construct_dict)

if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as rf:
        pprint.pprint(yaml.load(rf))
        pprint.pprint(mem)
