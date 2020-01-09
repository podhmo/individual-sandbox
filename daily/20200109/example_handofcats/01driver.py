import typing as t
from handofcats.injector import Injector


def hello(*, name: str = "world") -> None:
    print(f"hello {name}")


def byebye(name: str) -> None:
    print(f"byebye {name}")


class MultiDriver:
    def __init__(self):
        self.functions = []

    def register(self, fn: t.Callable[..., t.Any]) -> None:
        self.functions.append(fn)

    def create_parser(self, *, argv=None, description=None):
        if "--expose" in (argv or []):
            raise NotImplementedError("expose")
        else:
            import argparse

            return argparse.ArgumentParser()

    def run(self, argv=None, *, ignore_logging=False, description=None):
        parser = self.create_parser(argv=argv, description=description)
        subparsers = parser.add_subparsers()
        subparsers.required = True

        for fn in self.functions:
            sub_parser = subparsers.add_parser(fn.__name__)
            Injector(fn).inject(sub_parser)
            sub_parser.set_defaults(sub_command=fn)

        args = parser.parse_args(argv)
        return args


md = MultiDriver()
md.register(hello)
md.register(byebye)
print(md.run(["hello", "--name", "world"]))
