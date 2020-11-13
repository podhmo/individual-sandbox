from handofcats import as_subcommand


@as_subcommand
def issue(*, body: str, title: str, web: str) -> None:
    pass


if __name__ == "__main__":
    as_subcommand.run()
