import csv
import sys


def main(args):
    yield from run(args.input)


def run(itr):
    yield ["x", "x*x"]
    for x in itr:
        x = int(x)
        yield {"x": x, "x*x": x * x}


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", action="append", default=["1", "2", "3", "4", "5"])
    args = parser.parse_args()
    itr = main(args)
    w = csv.DictWriter(sys.stdout, fieldnames=next(itr))
    w.writerows(itr)
