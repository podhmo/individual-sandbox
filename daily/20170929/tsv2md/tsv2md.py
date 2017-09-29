import csv
from dictknife import loading


def to_table(rows):
    itr = iter(rows)
    row = next(itr)
    headers = list(row.keys())
    yield "|{}|".format("|".join(headers))
    yield "|{}|".format("|".join([":--" for _ in headers]))
    yield "|{}|".format("|".join([str(row.get(k, "-")) for k in headers]))
    for row in itr:
        yield "|{}|".format("|".join([str(row.get(k, "-")) for k in headers]))


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src", nargs="?", default=None)
    args = parser.parse_args()
    rows = loading.loadfile(args.src, format="tsv")
    print("\n".join(to_table(rows)))


if __name__ == "__main__":
    main()
