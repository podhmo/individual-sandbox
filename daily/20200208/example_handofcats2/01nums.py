from handofcats import as_command


@as_command
def nums() -> list:
    return list(range(10))
