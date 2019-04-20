import itertools
import sys
import argparse
from dictknife import loading


class PushBackIterator:
    def __init__(self, itr):
        self.itr = iter(itr)

    def pushback(self, x):
        self.itr = itertools.chain([x], self.itr)

    def __next__(self):
        return next(self.itr)

    def __iter__(self):
        return self


# type = comment | line | emptyline


def parse(o):
    s = PushBackIterator(o)

    def in_comment():
        r = []
        for line in s:
            if line.startswith("#"):
                r.append(line)
            elif not line.strip():
                r.append(line)
            else:
                s.pushback(line)
                break
        return {"type": "comment", "content": "\n".join(r)}

    def in_file():
        r = []
        for line in s:
            if line.startswith("#"):
                s.pushback(line)
                r.append(in_comment())
            else:
                r.append({"type": "line", "content": line})
        return r

    return in_file()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs="*", type=argparse.FileType("r"))
    args = parser.parse_args()

    files = args.files
    if len(files) == 0:
        files = [sys.stdin]
    for f in files:
        parsed = parse(f)
        loading.dumpfile(parsed)


if __name__ == "__main__":
    main()
