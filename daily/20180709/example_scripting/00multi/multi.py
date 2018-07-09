import sys
import itertools
from importlib.machinery import SourceFileLoader


def grouping(xs, p=lambda x: x != "--"):
    itr = iter(xs)
    while True:
        parsed = list(itertools.takewhile(p, itr))
        if not parsed:
            break
        yield parsed


def main():
    all_argv = sys.argv
    for argv in grouping(all_argv[1:]):
        sys.argv = argv
        SourceFileLoader("__main__", sys.argv[0]).load_module()
    print("end")


if __name__ == "__main__":
    main()
