import handofcats as h


@h.as_subcommand
def hello(*, name: str = "world"):
    print(f"hello {name}")


@h.as_subcommand
def byebye(name):
    print(f"byebye {name}")


h.as_subcommand.run()
