import sys


def div(x, y):
    print("{} / {} = {}".format(x, y, x / y))


def run(x, y):
    div(x, y)
    return 0


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-x", default=10, type=int, required=True)
    parser.add_argument("-y", default=2, type=int)
    parser.add_argument("--debug", action="store_true", default=False)

    args = parser.parse_args()
    try:
        sys.exit(run(args.x, args.y))
    except Exception as e:
        if args.debug:
            raise
        print("{e.__class__.__name__}: {e}".format(e=e), file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
