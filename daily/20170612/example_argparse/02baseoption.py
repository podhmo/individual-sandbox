import argparse
import logging
from collections import OrderedDict
logger = logging.getLogger(__name__)


def get_name(fn):
    return fn.__name__


class ParserProxy:
    def __init__(self, parser):
        self.parser = parser
        self._bind_subparsers(parser)

    def _bind_subparsers(self, parser):
        for action in parser._subparsers._group_actions:
            if hasattr(action, "add_parser"):
                self.__dict__.update(action.choices)

    def __getattr__(self, name):
        return getattr(self.parser, name)


class Registration:
    def __init__(self):
        self.registered = OrderedDict()

    def register(self, **kwargs):
        def _register(fn):
            self.registered[fn] = kwargs
            return fn

        return _register

    def create_parser(self, cls=argparse.ArgumentParser, proxy_cls=ParserProxy, **kwargs):
        parser = cls(**kwargs)
        subparsers = parser.add_subparsers(dest="subcommand")
        subparsers.required = True

        for fn, kwargs in self.registered.items():
            subparser = subparsers.add_parser(get_name(fn), **kwargs)
            subparser.set_defaults(fn=fn)
        return proxy_cls(parser)


registration = Registration()


@registration.register(description="this is foo")
def foo():
    return "foo"


@registration.register(description="this is bar")
def bar():
    return "bar"


if __name__ == "__main__":
    parser = registration.create_parser()

    loglevels = list(logging._nameToLevel.keys())
    parser.add_argument("--logging", choices=loglevels, default="INFO")

    parser.foo.add_argument("--name", required=True)

    parser.bar.add_argument("--type")

    args = parser.parse_args()
    logging.basicConfig(level=args.logging)

    print(args)

    logger.debug("hmm..")
    logger.info("hmm")
