import csv
import sys


def main(args):
    run(args.input)


def run(itr):
    w = csv.DictWriter(sys.stdout, fieldnames=["x", "x*x"])
    w.writeheader()
    for x in itr:
        x = int(x)
        w.writerow({"x": x, "x*x": x * x})


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", action="append", default=["1", "2", "3", "4", "5"])
    args = parser.parse_args()
    main(args)
