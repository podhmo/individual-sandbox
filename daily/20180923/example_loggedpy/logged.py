import typing as t
import sys
import os.path
from importlib import import_module
from importlib.machinery import SourceFileLoader
from importlib.util import (
    spec_from_file_location,
    find_spec,
)
from functools import partial
import shutil
import logging


class Driver:
    format = "%(levelname)s\t%(asctime)s\t%(message)s"
    out = sys.stderr
    level = logging.INFO

    def get_logger(self, name):
        return logging.getLogger(name)

    def setup(
        self,
        *,
        level: t.Optional[int] = None,
        out: t.Optional[t.IO] = None,
        format: t.Optional[str] = None
    ):
        logging.basicConfig(
            level=level or self.level,
            format=format or self.format,
            stream=out or self.out,
        )


def patch(logger):
    class Wrapper:
        def __init__(self, logger: logging.Logger, name: str) -> None:
            self.logger = logger
            self.name = name

        def write(self, s: str) -> None:
            if s.strip():
                getattr(self.logger, self.name)(s)

    sys.modules["builtins"].print = partial(print, file=Wrapper(logger, "info"))


def get_driver(path) -> Driver:  # using protocol (types)
    if path.startswith(":"):
        return globals()[path[1:]]()
    else:
        module, name = path.rsplit(":", 1)
        return getattr(import_module(module), name)()


def call_file(
    driver: Driver,  # using protocol (types)
    *,
    filepath: t.Optional[str],
    python_module: t.Optional[str],
    args: t.Sequence[str],
) -> None:
    if python_module is not None:
        # for: python -m <module>
        if filepath is not None:
            args.insert(0, filepath)
        spec = find_spec(python_module)
        sys.argv[1:] = args
        driver.setup(level=logging.DEBUG)  # xxx
        patch(driver.get_logger(spec.name))
        return SourceFileLoader("__main__", spec.origin).load_module()
    elif os.path.exists(filepath) and not os.path.isdir(filepath):
        # for: python <file>
        spec = spec_from_file_location("__main__", filepath)
        sys.argv[1:] = args
        driver.setup(level=logging.DEBUG)  # xxx
        patch(driver.get_logger(spec.name))
        return SourceFileLoader("__main__", spec.origin).load_module()
    else:
        # for: <command>
        cmdpath = shutil.which(filepath)
        if not cmdpath:
            raise RuntimeError(f"not supported: {sys.argv}")

        sys.argv[1:] = args
        driver.setup(level=logging.DEBUG)  # xxx
        patch(driver.get_logger(os.path.basename(cmdpath)))
        return SourceFileLoader("__main__", cmdpath).load_module()


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("filepath", nargs="?")
    parser.add_argument("-m", "--python-module")
    parser.add_argument("--driver", default=":Driver")
    args, extras = parser.parse_known_args()

    if args.filepath is None and args.python_module is None:
        return parser.print_help()
    driver = get_driver(args.driver)

    return call_file(driver, filepath=args.filepath, python_module=args.python_module, args=extras)


if __name__ == "__main__":
    main()
