from handofcats import as_subcommand


@as_subcommand
def hello():
    print("hello")


@as_subcommand
def bye():
    print("bye")


as_subcommand.run()
