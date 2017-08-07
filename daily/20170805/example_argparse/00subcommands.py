def foo():
    pass


def bar():
    pass


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="subcommand")
    subparsers.required = True

    foo_parser = subparsers.add_parser("foo")
    foo_parser.set_defaults(fn=foo)

    bar_parser = subparsers.add_parser("bar")
    bar_parser.set_defaults(fn=bar)

    args = parser.parse_args()
    print(args)
