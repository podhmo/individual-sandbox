import argparse
import re
import os


def parse(root, rx=re.compile('go/((?:[^/" \.:]+/)*[^/" \.:]+)')):
    for r, ds, fs in os.walk(root):
        for f in fs:
            if "_test.go" in f:
                continue
            fpath = os.path.join(r, f)
            with open(fpath) as rf:
                for line in rf:
                    m = rx.search(line)
                    if m is not None:
                        print(m.group().strip())


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("root")
    args = parser.parse_args()
    parse(args.root)
