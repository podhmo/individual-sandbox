from gogen import runtime


def hello(*, name: str) -> None:
    """hello message"""
    from gogen.generate import cli

    with runtime.generate(cli):
        runtime.printf("hello %s\n", name)


if __name__ == "__main__":
    runtime.main(name=__name__, here=__file__, root="cmd")
