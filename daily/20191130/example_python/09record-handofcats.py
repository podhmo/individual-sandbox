from handofcats.parsers.expose import CallbackArgumentParser


def foo(*, x: str, y: str) -> None:
    print(f"foo x={x} y={y}")


def bar(*, z: str) -> None:
    print(f"bar z={z}")


def print_argparse_code(fn: callable, history: list, *, outname: str = "main"):
    print(history)
    pass


parser = CallbackArgumentParser(print_argparse_code, None)
subparsers = parser.add_subparsers(required=True, title="actions")

foo_parser = subparsers.add_parser("foo")
foo_parser.add_argument("-x")
foo_parser.add_argument("-y")
foo_parser.set_defaults(action=foo)

bar_parser = subparsers.add_parser("bar")
bar_parser.add_argument("-z")
bar_parser.set_defaults(action=bar)
