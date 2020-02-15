import dataclasses
from monogusa import component


@dataclasses.dataclass
class Foo:
    name: str


@component
def foo() -> Foo:
    return Foo(name="foo")


def run(foo: Foo) -> None:
    print("!", foo)


if __name__ == "__main__":
    from monogusa import cli

    cli.run()
