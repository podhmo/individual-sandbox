import sys


class FakeArgumentParser:
    def __init__(self, *args, history=None, **kwargs):
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

        def _make_args(history, default=""):
            name = history["name"]
            if name == "__init__":
                name = default
            kwargs = {k: repr(v) for k, v in history["kwargs"].items()}
            args = [repr(v) for v in history["args"]]
            return f"{name}({LazyArgumentsAndKeywords(args, kwargs)})"

        m = Module()
        with m.def_("Main"):
            m.import_("argparse")
            m.stmt(f"parser = argparse.ArgumentParser{_make_args(self.history[0])}")
            for x in self.history[1:-1]:
                m.stmt(f"parser.{_make_args(x)}")
            m.stmt(f"args = parser.{_make_args(self.history[-1])}")
            m.stmt("main(**vars(args))")

        with m.if_("__name__ == '__main__'"):
            m.stmt("Main()")

        print(m)
        sys.exit(0)


def parser(parser):
    import logging
    parser.add_argument("-c", "--config", required=True)
    parser.add_argument("--log-level", default="INFO", choices=tuple(logging._nameToLevel.keys()))
    args = parser.parse_args()
    run(args)


parser(FakeArgumentParser(help="fake"))
