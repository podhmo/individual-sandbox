import pandas


def main(args):
    base = pandas.read_csv(args.basefile)
    for f in args.files:
        df = pandas.read_csv(f)
        base = base.merge(df, how="left", on=args.on)
    print(base.to_csv(index=False))


def includeme(config):
    config.settings["action"] = main
