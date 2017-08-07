import contextlib


@contextlib.contextmanager
def subparser(subparsers, fn, *args, **kwargs):
    parser = subparsers.add_parser(fn.__name__, *args, **kwargs)
    dests = []
    arrived = set()

    def add_argument(*args, **kwargs):
        ac = parser.add_argument(*args, **kwargs)
        if ac.dest not in arrived:
            arrived.add(ac.dest)
            dests.append(ac.dest)
        return ac

    yield add_argument

    def run(args):
        return fn(**{name: getattr(args, name) for name in dests})

    parser.set_defaults(fn=run)


def foo(*, x, y):
    print("foo", x, y)


def bar(*, x, y, z):
    print("bar", x, y, z)


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_subparsers
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="subcommand")
    subparsers.required = True

    with subparser(subparsers, foo) as add_argument:
        add_argument("-y", default=None)
        add_argument("-x", default=None)


    with subparser(subparsers, bar) as add_argument:
        add_argument("-x", default=None)
        add_argument("-y", default=None)
        add_argument("-z", default=None)

    args = parser.parse_args()
    return args.fn(args)


if __name__ == "__main__":
    main()
