from egoist.generators.clikit import runtime, clikit


def hello(*, name: str) -> None:
    """hello message"""
    with runtime.generate(clikit):
        runtime.printf("hello %s\n", name)


def byebye(*, name: str) -> None:
    """byebye message"""
    pass
