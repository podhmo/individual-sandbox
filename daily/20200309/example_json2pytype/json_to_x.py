from handofcats import as_subcommand


@as_subcommand
def scan(filename: str) -> None:
    from dictknife import loading
    from detector import show

    d = loading.loadfile("data/config.json")
    show(d)


as_subcommand.run()
