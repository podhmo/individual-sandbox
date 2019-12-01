import typing as t
from types import ModuleType
import sys
import argparse
import inspect
from handofcats.util import reify
from handofcats.injector import Injector

# TODO: expose


class Driver:
    def __init__(
        self,
        *,
        parser: t.Optional[argparse.ArgumentParser] = None,
        title: str = "subcommands",
    ):
        self.parser = parser or argparse.ArgumentParser()
        self.title = title

    @reify
    def subparsers(self):
        return self.parser.add_subparsers(
            required=True, title=self.title, dest="action"
        )

    def register(self, fn: t.Callable) -> t.Callable:
        sub_parser = self.subparsers.add_parser(fn.__name__, help=inspect.getdoc(fn))
        Injector(fn).inject(sub_parser)
        sub_parser.set_defaults(action=fn)
        return fn

    def _run(self, argv: t.Optional[t.List[str]] = None) -> t.Any:
        args = self.parser.parse_args(argv)
        params = vars(args)
        action = params.pop("action")
        return action(**params)

    def run(
        self,
        argv: t.Optional[t.List[str]] = None,
        *,
        aggressive: bool = False,
        where: t.Optional[str] = None,
        module: t.Optional[ModuleType] = None,
        _depth: int = 1,
    ):
        if aggressive or module:
            if module is not None:
                _globals = module.__dict__
            else:
                frame = sys._getframe(_depth)  # black magic
                _globals = frame.f_globals
            where = _globals["__name__"]
            for name, v in _globals.items():
                if name.startswith("_"):
                    continue
                if inspect.isfunction(v) and v.__module__ == where:
                    self.register(v)
        return self._run(argv)


def main():
    from magicalimport import import_module

    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("file")
    args, rest = parser.parse_known_args()
    m = import_module(args.file)
    Driver().run(rest, module=m)


if __name__ == "__main__":
    main()
