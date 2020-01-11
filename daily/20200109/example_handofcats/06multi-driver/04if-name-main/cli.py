from handofcats import as_subcommand


@as_subcommand
def hello(*, name: str = "world"):
    print(f"hello {name}")


@as_subcommand
def byebye(name):
    print(f"byebye {name}")


if __name__ == "__main__":
    as_subcommand.run()
