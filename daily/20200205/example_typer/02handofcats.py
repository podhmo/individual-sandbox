from handofcats import as_subcommand


@as_subcommand
def hello(name: str):
    print(f"Hello {name}")


@as_subcommand
def goodbye(name: str, formal: bool = False):
    if formal:
        print(f"Goodbye Ms. {name}. Have a good day.")
    else:
        print(f"Bye {name}!")


as_subcommand.run()
