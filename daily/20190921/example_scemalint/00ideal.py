def ideal(filename: str, *, schema: t.Optional[str] = None):
    filepath = os.path.abspath(filename)

    loader = get_loader(filepath)
    stream = from_loader(loader)

    if schema is None:
        schemapath = os.path.abspath(schema)
        validator = get_validator(loading.loadfile(schemapath), check_schema=True)
        stream = stream.add_validator(validator)

    formatter = get_formatter(filepath, loader=loader)
    for err in stream:
        print(formatter.format(err))
