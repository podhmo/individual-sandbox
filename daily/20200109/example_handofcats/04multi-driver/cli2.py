import handofcats


@handofcats.as_subcommand
def hello(*, name: str = "world"):
    print(f"hello {name}")


@handofcats.as_subcommand
def byebye(name):
    print(f"byebye {name}")


handofcats.as_subcommand.run()
