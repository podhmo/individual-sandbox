import sys


def run(*, n: int = 1):
    for i, line in zip(range(n), sys.stdin):
        sys.stderr.write(line)
    for line in sys.stdin:
        sys.stdout.write(line)


def main(argv=None):
    import argparse

    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument("-n", required=False, default=1, type=int, help="(default: 1)")
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == "__main__":
    main()
