import sys
import itertools
try:
    import csvresumable as csv
except ImportError:
    import csv


def collect(itr):
    for x, y in itr:
        x = int(x)
        y = int(y)
        yield {"id": x, "x": x * x, "y": y}


def run(*, config, size=None, x_list=None, y_list=None, input=None, decoration=None):
    if input is not None:
        r = ((row["x"], row["y"]) for row in csv.DictReader(input))
    else:
        r = zip((x_list or []), (y_list or []))

    if size is not None:
        r = itertools.islice(r, size)

    itr = collect(r)
    if decoration is not None:
        itr = decoration(itr)
    itr = decoration_for_calculation(itr)
    yield from itr


def decoration_for_calculation(itr):
    for row in itr:
        if row["y"] == 0:
            yield row
            continue
        row["x_y_rate"] = row["x"] / row["y"]
        yield row


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--config", required=True)
    parser.add_argument("-x", action="append", dest="x_list")
    parser.add_argument("-y", action="append", dest="y_list")
    parser.add_argument("--size", type=int)
    parser.add_argument("--input", type=argparse.FileType("r"))

    args = parser.parse_args()
    itr = run(**vars(args))

    w = csv.DictWriter(sys.stdout, fieldnames=["id", "x", "y", "x_y_rate"], restval="-")
    w.writeheader()
    w.writerows(itr)


if __name__ == "__main__":
    main()
