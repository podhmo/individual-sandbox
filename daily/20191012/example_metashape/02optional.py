import typing as t
import typing_extensions as tx
import typing_inspect

JSONSchemaType = tx.Literal["boolean", "string", "integer", "number", "object", "array"]


class TypeInfoDict(tx.TypedDict, total=True):
    type: JSONSchemaType
    optional: bool


def resolve_type_info(
    typ: t.Type[t.Any], *, strict: bool = True, _nonetype=type(None)
) -> TypeInfoDict:
    optional = False
    if hasattr(typ, "__origin__"):  # xxx
        args = typing_inspect.get_args(typ)
        if len(args) == 2:
            if args[0] == _nonetype:
                optional = True
                typ = args[1]
            elif args[1] == _nonetype:
                optional = True
                typ = args[0]

    if issubclass(typ, str):
        return {"type": "string", "optional": optional}
    elif issubclass(typ, bool):
        return {"type": "boolean", "optional": optional}
    elif issubclass(typ, int):
        return {"type": "integer", "optional": optional}
    elif issubclass(typ, float):
        return {"type": "number", "optional": optional}
    elif hasattr(typ, "keys"):
        return {"type": "object", "optional": optional}
    elif issubclass(typ, (list, tuple)):
        return {"type": "array", "optional": optional}
    elif strict:
        raise ValueError("unsupported {!r}".format(typ))
    else:
        # logger.info("unsupported type is found: %r", typ)
        return {"type": "object", "optional": False}


print(resolve_type_info(int))
print(dir(t.Optional[int]))
print(resolve_type_info(t.Optional[int]))
# print(ti.get_args(t.Optional[int]))
# print(resolve_type(t.Optional[int]))
