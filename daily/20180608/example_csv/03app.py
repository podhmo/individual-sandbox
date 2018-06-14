import pandas


def main(args):
    base = pandas.read_csv(args.basefile)
    for f in args.files:
        df = pandas.read_csv(f)
        base = base.merge(df, how="left", on=args.on)
    print(base.to_csv(index=False))


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("basefile")
    parser.add_argument("files", nargs="+")
    parser.add_argument("--on", required=True)
    args = parser.parse_args()
    main(args)
