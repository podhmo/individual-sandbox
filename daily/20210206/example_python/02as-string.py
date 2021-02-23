from __future__ import annotations
from metashape.typeinfo import typeinfo
import typing as t

if t.TYPE_CHECKING:
    from metashape.typeinfo import TypeInfo


NoneType = type(None)

# todo: None?


def type_string(
    typ: t.Type[t.Any], to_str: t.Optional[t.Callable[TypeInfo], str] = None
) -> str:
    """
    >>> type_string(str)
    'str'
    >>> type_string(int)
    'int'

    >>> import typing
    >>> type_string(typing.Optional[int])
    'int?'

    >>> type_string(typing.List)
    'list[Any]'
    >>> type_string(typing.List[str])
    'list[str]'
    >>> type_string(typing.List[typing.List[str]])
    'list[list[str]]'

    >>> type_string(typing.List[typing.Optional[str]])
    'list[str?]'
    >>> type_string(typing.Optional[typing.List[str]])
    'list[str]?'

    >>> type_string(typing.Dict[str, int])
    'dict[str, int]'
    >>> type_string(typing.Dict[str, typing.Set[int]])
    'dict[str, set[int]]'

    >>> class A: pass;
    >>> type_string(A)
    'A'
    >>> type_string(t.Optional[A])
    'A?'
    """
    return _type_string(typeinfo(typ), to_str=to_str or _to_str_default)


def _to_str_default(info: TypeInfo) -> str:
    return info.type_.__name__


def _type_string(info: TypeInfo, to_str: t.Callable[[TypeInfo], str]) -> str:
    if info.is_optional:
        return f"{type_string(info.type_, to_str=to_str)}?"
    if info.is_container:
        if len(info.args) == 0:
            args_str = "Any"
        else:
            args_str = ", ".join(_type_string(x, to_str=to_str) for x in info.args)
        return f"{info.container_type}[{args_str}]"
    return to_str(info)


def run(typ: t.Type[t.Any]) -> None:
    print(typ, type_string(typ))


if __name__ == "__main__":
    import doctest

    doctest.testmod()
