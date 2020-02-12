import typing as t


def typestr(
    typ: t.Type[t.Any],
    *,
    nonetype: t.Type[t.Any] = type(None),
    here: t.Optional[str] = None,
) -> str:
    # builtins
    if typ.__module__ == "builtins":
        if typ.__name__ == "NoneType":
            return "None"
        else:
            return typ.__name__

    # TODO: imported
    # TODO: imported with alias
    # get prefix, get aliases
    prefix = "x"
    # same module
    if hasattr(typ, "__origin__"):  # for typing.Union, typing.Optional, ...
        args = typ.__args__
        # for Optional
        if typ.__origin__ == t.Union and len(args) == 2:
            args = [x for x in args if x is not nonetype]
            if len(args) == 1:
                return f"{prefix}.Optional[{typestr(args[0])}]"
        name = getattr(typ, "_name") or getattr(typ.__origin__, "_name")
        return f"{prefix}.{name}[{', '.join(typestr(x) for x in args)}]"
    return str(typ)
