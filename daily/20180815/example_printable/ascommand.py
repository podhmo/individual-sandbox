import sys
import inspect
import argparse
import re


class FakeArgumentParser:
    def __init__(self, fn, *args, history=None, **kwargs):
        self.fn = fn
        self.history = history or [{"name": "__init__", "args": args, "kwargs": kwargs}]

    def __getattr__(self, name):
        self.history.append({"name": name})
        return self

    def __call__(self, *args, **kwargs):
        latest = self.history[-1]
        assert "args" not in latest
        latest["args"] = args
        latest["kwargs"] = kwargs

    def parse_args(self, *args, **kwargs):
        self.history.append({"name": "parse_args", "args": args, "kwargs": kwargs})
        from prestring.python import Module, LazyArgumentsAndKeywords

        def _make_call_stmt(history, default=""):
            name = history["name"]
            if name == "__init__":
                name = default
            kwargs = {k: repr(v) for k, v in history["kwargs"].items()}
            args = [repr(v) for v in history["args"]]
            return f"{name}({LazyArgumentsAndKeywords(args, kwargs)})"

        m = Module()
        m.sep()
        with m.def_("main"):
            m.import_("argparse")
            m.stmt(f"parser = argparse.ArgumentParser{_make_call_stmt(self.history[0])}")
            m.stmt("parser.print_usage = parser.print_help")
            for x in self.history[1:-1]:
                m.stmt(f"parser.{_make_call_stmt(x)}")
            m.stmt(f"args = parser.{_make_call_stmt(self.history[-1])}")
            m.stmt(f"{self.fn.__name__}(**vars(args))")

        with m.if_("__name__ == '__main__'"):
            m.stmt("main()")

        with open(inspect.getsourcefile(self.fn)) as rf:
            source = rf.read()
        rx = re.compile("(?:@([\S]+\.)?as_command.*|^.*import ascommand.*)\n", re.MULTILINE)
        exposed = rx.sub("", "".join(source))
        print(exposed)
        print(m)
        sys.exit(0)


def as_command(fn=None, argv=None, level=2):
    def call(fn, level=1, argv=argv):
        frame = sys._getframe(level)
        name = frame.f_globals["__name__"]
        if name == "__main__":
            argspec = inspect.getfullargspec(fn)
            if "--expose" in (argv or sys.argv):
                parser = FakeArgumentParser(fn, description=fn.__doc__)
            else:
                parser = argparse.ArgumentParser(description=fn.__doc__)
                parser.add_argument("--expose", action="store_true")
                parser.print_usage = parser.print_help

            for k in argspec.kwonlyargs:
                parser.add_argument(
                    f'{"-" if len(k) <= 1 else "--"}{k.replace("_", "-")}', required=True
                )
            args = parser.parse_args(argv)
            params = vars(args).copy()
            params.pop("expose")
            return fn(**params)
        else:
            return fn

    if fn is None:
        return call
    else:
        return call(fn, level=level, argv=argv)
