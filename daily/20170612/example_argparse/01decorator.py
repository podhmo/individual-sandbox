import argparse
from collections import OrderedDict


class Registration:
    def __init__(self):
        self.registered = OrderedDict()

    def register(self, **kwargs):
        def _register(fn):
            self.registered[fn] = kwargs
            return fn

        return _register

    def create_parser(self, **kwargs):
        parser = argparse.ArgumentParser(**kwargs)
        subparsers = parser.add_subparsers(dest="subcommand")
        subparsers.required = True

        for fn, kwargs in self.registered.items():
            subparser = subparsers.add_parser(fn.__name__, **kwargs)
            subparser.set_defaults(fn=fn)
        return parser


registration = Registration()


@registration.register(description="this is foo")
def foo():
    return "foo"


@registration.register(description="this is bar")
def bar():
    return "bar"


if __name__ == "__main__":
    parser = registration.create_parser()

    args = parser.parse_args()
    print(args)
