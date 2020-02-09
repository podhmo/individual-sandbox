from handofcats import as_subcommand


@as_subcommand
def foo():
    print("foo")


@as_subcommand
def bar():
    print("bar")


as_subcommand.run()
