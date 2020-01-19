from handofcats import as_subcommand


@as_subcommand
def foo(target: str):
    import heavy_module

    heavy_module.hello(target)


@as_subcommand
def bar(target: str):
    import heavy_module

    heavy_module.hello(target)


as_subcommand.run()
