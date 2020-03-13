from handofcats import as_subcommand


@as_subcommand
def scan(filename: str) -> None:
    from dictknife import loading
    from detector import show

    d = loading.loadfile(filename)
    show(d)


@as_subcommand
def emit(filename: str) -> None:
    from dictknife import loading
    from detector import detect, Object

    d = loading.loadfile(filename)
    result = detect(d)
    for info in reversed(result.history):
        if isinstance(info, Object):
            print(info)


as_subcommand.run()
