from egoist import runtime
from magicalimport import import_module

internal = import_module("./internal.py", here=__file__)


def hello(*, config: str) -> None:
    """hello message"""
    from egoist.generate.clikit import clikit
    from egoist.internal.graph import Builder
    from egoist.generate.go import di

    with runtime.generate(clikit) as m:
        b = Builder()
        b.add_node(**di.parse(internal.NewConfig))

        g = b.build()
        variables = di.primitives(g, {**locals(), "filename": config})
        config = di.inject(m, g, variables=variables)

        fmt = m.import_("fmt")
        m.stmt(fmt.Println(config.LogLevel))


if __name__ == "__main__":
    runtime.main(name=__name__, here=__file__, root="cmd")
