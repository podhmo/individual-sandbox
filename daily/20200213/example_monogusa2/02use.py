from monogusa import default_component


class Foo:
    pass


@default_component
def foo() -> Foo:
    return Foo()


def use(f: Foo) -> None:
    print(f)


if __name__ == "__main__":
    from monogusa.cli import run

    run()
