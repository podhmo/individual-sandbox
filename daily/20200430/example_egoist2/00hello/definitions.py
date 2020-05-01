from egoist import runtime


def hello(*, name: str) -> None:
    """hello message"""
    from egoist.generate.clikit import clikit

    with runtime.generate(clikit):
        runtime.printf("hello %s\n", name)


if __name__ == "__main__":
    runtime.main(name=__name__, here=__file__, root="cmd")
