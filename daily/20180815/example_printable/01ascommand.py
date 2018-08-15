import inspect
import sys
import argparse


def as_command(fn=None, argv=None, level=2):
    def call(fn, level=1, argv=argv):
        frame = sys._getframe(level)
        name = frame.f_globals["__name__"]
        if name == "__main__":
            argspec = inspect.getfullargspec(fn)
            parser = argparse.ArgumentParser()
            for k in argspec.kwonlyargs:
                parser.add_argument(
                    f'{"-" if len(k) <= 1 else "--"}{k.replace("_", "-")}', required=True
                )
            args = parser.parse_args()
            return fn(**vars(args))
        else:
            return fn

    if fn is None:
        return call
    else:
        return call(fn, level=level, argv=argv)


@as_command
def run(*, x: int, y: int):
    print(f"{x} + {y} = {x + y}")
