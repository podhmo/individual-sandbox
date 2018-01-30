import argparse
import os
from collections import defaultdict


def aggregate(root):
    d = {}
    for fpath in os.listdir(root):
        k = os.path.splitext(os.path.basename(fpath))[0].replace("_", "/")
        if "testdata" in k:
            continue
        if "internal" in k:
            continue
        deps = d[k] = []
        with open(os.path.join(root, fpath)) as rf:
            for line in rf:
                if "testdata" in line:
                    continue
                if "internal" in line:
                    continue
                if k == line.strip():
                    continue
                deps.append(line.strip())
    return d


def todot(d):
    i = 0

    def c():
        nonlocal i
        i += 1
        return "g{}".format(i)

    ids = defaultdict(c)
    print("digraph g {")
    for src, deps in sorted(d.items()):
        for k in [src, *deps]:
            if k not in ids:
                print('  {} [label="{}"]'.format(ids[k], k))
    for src, deps in sorted(d.items()):
        for dst in deps:
            print('  {} -> {}'.format(ids[src], ids[dst]))
    print("}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("root")
    args = parser.parse_args()
    todot(aggregate(args.root))
