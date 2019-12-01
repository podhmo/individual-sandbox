from capturedmock import CapturedMock, compile, scan, squash


def foo(*, x: str, y: str) -> None:
    print(f"foo x={x} y={y}")


def bar(*, z: str) -> None:
    print(f"bar z={z}")


def print_argparse_code(fn: callable, history: list, *, outname: str = "main"):
    print(history)
    pass


argparse = CapturedMock()
parser = argparse.ArgumentParser()
subparsers = parser.add_subparsers(required=True, title="actions")

foo_parser = subparsers.add_parser("foo")
foo_parser.add_argument("-x")
foo_parser.add_argument("-y")
foo_parser.set_defaults(action=foo)

bar_parser = subparsers.add_parser("bar")
bar_parser.add_argument("-z")
bar_parser.set_defaults(action=bar)

for line in compile(scan(squash(parser.history))):
    print(line)
