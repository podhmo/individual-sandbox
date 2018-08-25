import typing as t
import argparse
if t.TYPE_CHECKING:
    import myprotocol  # noqa


class FakeParser:
    def parse_args(
        self,
        argv: t.Optional[t.Sequence[str]] = None,
        namespace: t.Optional[argparse.Namespace] = None
    ) -> t.Any:
        pass

    def add_argument(
        self,
        *name_or_flags: str,
        **kwarg: t.Any,
    ) -> t.Any:
        pass


def use(parser: "myprotocol.ArgumentParserSubset") -> None:
    parser.add_argument("--verbose", action="store_true")
    print(parser.parse_args())


def main() -> None:
    use(argparse.ArgumentParser())
    use(FakeParser())
