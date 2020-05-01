from egoist import runtime


def hello(*, name: str) -> None:
    """hello message"""
    from egoist.generate.clikit import clikit

    with runtime.generate(clikit) as m:
        args = runtime.get_cli_rest_args()
        target = m.symbol("target")
        with m.for_(f"_, {target} := range {args}"):
            runtime.printf("%s: hello %s\n", name, target)
def bye(*, name: str) -> None:
    """bye message"""
    from egoist.generate.clikit import clikit

    with runtime.generate(clikit) as m:
        args = runtime.get_cli_rest_args()
        target = m.symbol("target")
        with m.for_(f"_, {target} := range {args}"):
            runtime.printf("%s: bye %s\n", name, target)


if __name__ == "__main__":
    runtime.main(name=__name__, here=__file__, root="cmd")
