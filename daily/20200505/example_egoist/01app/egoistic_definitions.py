from egoist import App

# setup
app = App(cmd_root="./cmd")
app.include("egoist.directives.define_cli")


@app.define_cli
def hello(*, name: str) -> None:
    """hello message"""
    from egoist import runtime
    from egoist.generate import clikit

    with runtime.generate(clikit):
        runtime.printf("hello %s\n", name)


if __name__ == "__main__":
    app.run(name=__name__, here=__file__)
