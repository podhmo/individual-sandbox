from gogen import runtime


def hello(*, name: str) -> None:
    """hello message"""
    from gogen.generate import cli

    with runtime.generate(cli) as m:
        hello = m.import_("m/hello")
        m.stmt(hello.Hello(name))


if __name__ == "__main__":
    runtime.main(name=__name__, here=__file__, root="cmd")
