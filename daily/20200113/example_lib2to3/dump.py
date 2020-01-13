from handofcats import as_command


@as_command
def run(filename: str):
    from prestring.python.parse import PyTreeDumper, parse_file

    PyTreeDumper().visit(parse_file(filename))
