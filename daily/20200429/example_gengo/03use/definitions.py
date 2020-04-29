from egoist import runtime
from magicalimport import import_module

internal = import_module("./internal.py", here=__file__)


def use(*, grumby: bool) -> None:
    """hello message"""
    from egoist.generate.clikit import clikit
    from egoist.generate.go import di
    from egoist.internal.graph import Builder

    args = runtime.get_args()
    args.grumby.help = "grumby? what is this?"

    with runtime.generate(clikit) as m:
        b = Builder()

        b.add_node(**di.parse(internal.NewEvent))
        b.add_node(**di.parse(internal.NewGreeter))
        b.add_node(**di.parse(internal.NewMessage))

        g = b.build()

        event = di.inject(m, g, variables=di.primitives(g, locals()))
        m.stmt(event.Start())


if __name__ == "__main__":
    runtime.main(name=__name__, here=__file__, root="")
