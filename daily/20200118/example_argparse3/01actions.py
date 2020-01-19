import typing as t
from handofcats import as_subcommand


@as_subcommand
def foo(targets: t.List[str]):
    import heavy_module

    for target in targets:
        heavy_module.hello(target)


@as_subcommand
def bar(targets: t.List[str]):
    import heavy_module

    for target in targets:
        heavy_module.hello(target)


as_subcommand.run()
