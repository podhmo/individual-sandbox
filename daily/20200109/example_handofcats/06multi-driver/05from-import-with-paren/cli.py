from handofcats import (
    as_command,
    as_subcommand,
)


@as_subcommand
def hello(*, name: str = "world"):
    print(f"hello {name}")


@as_subcommand
def byebye(name):
    print(f"byebye {name}")


as_subcommand.run()
