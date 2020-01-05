import time
import typing as t


def type_str(
    typ: t.Type[t.Any],
    *,
    nonetype: t.Type[t.Any] = type(None),
    aliases: t.Dict[str, str] = {"typing": "t"},
) -> str:
    if typ == ...:
        return "..."
    if typ.__module__ == "builtins":
        if typ.__name__ == "NoneType":
            return "None"
        else:
            return typ.__name__

    if hasattr(typ, "__name__"):
        return f"{aliases.get(typ.__module__, typ.__module__)}.{typ.__name__}"
    elif hasattr(typ, "__origin__"):  # for typing.Union, typing.Optional, ...
        prefix = aliases.get(typ.__module__, typ.__module__)
        args = typ.__args__
        if typ.__origin__ == t.Union and len(args) == 2:
            args = [x for x in args if x is not nonetype]
            if len(args) == 1:
                return f"{prefix}.Optional[{type_str(args[0], aliases=aliases)}]"
        name = getattr(typ, "_name") or getattr(typ.__origin__, "_name")
        return (
            f"{prefix}.{name}[{', '.join(type_str(x, aliases=aliases) for x in args)}]"
        )
    return str(typ).replace(
        typ.__module__ + ".", aliases.get(typ.__module__, typ.__module__) + ".",
    )  # xxx


def run(typ: t.Type[t.Any]) -> str:
    return f"{typ} -> {type_str(typ)}"


# fmt: off
run(int)  # => "<class 'int'> -> int"
run(t.Optional[int])  # => 'typing.Union[int, NoneType] -> t.Optional[int]'
run(t.Optional[time.time])  # => 'typing.Union[<built-in function time>, NoneType] -> t.Optional[time.time]'
run(t.Tuple[int, str])  # => 'typing.Tuple[int, str] -> t.Tuple[int, str]'
run(t.Tuple[int, ...])  # => 'typing.Tuple[int, ...] -> t.Tuple[int, ...]'
run(t.Dict[str, t.Dict[str, t.Optional[int]]]) # => 'typing.Dict[str, typing.Dict[str, typing.Union[int, NoneType]]] -> t.Dict[str, t.Dict[str, t.Optional[int]]]'
import model
run(t.Optional[model.Person]) # => 'typing.Union[model.Person, NoneType] -> t.Optional[model.Person]'
# fmt: on
