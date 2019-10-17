import typing as t
from functools import lru_cache
import typing_extensions as tx
import typing_inspect


# TODO: support inheritance
# TODO: extract description

ContainerType = tx.Literal["list", "tuple", "dict", "union"]


class Container(tx.TypedDict, total=True):
    raw: t.Type[t.Any]  # t.Optional[t.List[int]] -> t.Optional[t.List[int]]
    container: ContainerType
    args: t.Tuple["TypeInfo", ...]

    is_optional: bool  # t.Optional[int] -> True, int -> False
    is_composite: bool  # t.Union -> True, dict -> False


class Atom(tx.TypedDict, total=True):
    raw: t.Type[t.Any]  # t.Optional[int] -> t.Optional[int]
    underlying: t.Type[t.Any]  # t.Optionall[int] -> int

    is_optional: bool  # t.Optional[int] -> True, int -> False
    custom: t.Optional[
        t.Type[t.Any]
    ]  # int -> None, Person -> Person, t.List[int] -> None


TypeInfo = t.Union[Atom, Container]


def _make_atom(
    *,
    raw: t.Type[t.Any],
    underlying: t.Type[t.Any],
    is_optional: bool = False,
    custom: t.Optional[t.Any] = None,
) -> Atom:
    return {
        "raw": raw,
        "underlying": underlying,
        "is_optional": is_optional,
        "custom": custom,
    }


def _make_container(
    *,
    container: ContainerType,
    raw: t.Type[t.Any],
    args: t.Tuple["TypeInfo", ...],
    is_optional: bool = False,
    is_composite: bool = False,
) -> Container:
    return {
        "raw": raw,
        "args": tuple(args),
        "container": container,
        "is_optional": is_optional,
        "is_composite": is_composite,
    }


@lru_cache(maxsize=128, typed=False)
def resolve_type_info(
    typ: t.Type[t.Any],
    *,
    is_optional: bool = False,
    custom: t.Optional[t.Type[t.Any]] = None,
    _nonetype: t.Type[t.Any] = t.cast(t.Type[t.Any], type(None)),  # xxx
    _anytype: t.Type[t.Any] = t.cast(t.Type[t.Any], t.Any),  # xxx
    _primitives: t.Set[t.Type[t.Any]] = t.cast(
        t.Set[t.Type[t.Any]], set([str, int, bool, str, bytes, dict, list, t.Any])
    ),
) -> TypeInfo:
    raw = typ
    args = typing_inspect.get_args(typ)
    underlying = getattr(typ, "__origin__", None)

    if underlying is None:
        if not hasattr(typ, "__iter__"):
            underlying = typ  # xxx
        elif issubclass(typ, str):
            underlying = typ
        elif issubclass(typ, t.Sequence):
            return _make_container(
                raw=raw,
                container="tuple" if issubclass(typ, tuple) else "list",
                args=(resolve_type_info(_anytype),),
            )
        elif issubclass(typ, t.Mapping):
            childinfo = resolve_type_info(_anytype)
            return _make_container(
                raw=raw, container="dict", args=(childinfo, childinfo)
            )
        else:
            underlying = typ  # xxx
    else:
        if underlying == t.Union:
            if len(args) == 2:
                if args[0] == _nonetype:
                    is_optional = True
                    typ = underlying = args[1]
                elif args[1] == _nonetype:
                    is_optional = True
                    typ = underlying = args[0]
                else:
                    return _make_container(
                        container="union",
                        raw=raw,
                        args=tuple([resolve_type_info(t) for t in args]),
                        is_optional=is_optional,
                        is_composite=True,
                    )
            else:
                is_optional = _nonetype in args
                if is_optional:
                    args = [x for x in args if x != _nonetype]
                return _make_container(
                    container="union",
                    raw=raw,
                    args=tuple([resolve_type_info(t) for t in args]),
                    is_optional=is_optional,
                    is_composite=True,
                )

        if hasattr(typ, "__origin__"):
            underlying = typ.__origin__
            if underlying == tx.Literal:
                args = typing_inspect.get_args(typ)
                underlying = type(args[0])  # TODO: meta info
            elif issubclass(underlying, t.Sequence):
                args = typing_inspect.get_args(typ)
                return _make_container(
                    raw=raw,
                    container="tuple" if issubclass(underlying, tuple) else "list",
                    args=tuple([resolve_type_info(t) for t in args]),
                    is_optional=is_optional,
                )
            elif issubclass(underlying, t.Mapping):
                args = typing_inspect.get_args(typ)
                return _make_container(
                    raw=raw,
                    container="dict",
                    args=tuple([resolve_type_info(t) for t in args]),
                    is_optional=is_optional,
                )
            else:
                raise ValueError(f"unsuported type %{typ}")

    while hasattr(underlying, "__supertype__"):
        underlying = underlying.__supertype__

    if underlying not in _primitives:
        custom = underlying
    return _make_atom(
        raw=raw, underlying=underlying, is_optional=is_optional, custom=custom
    )


def resolve_type(typ: t.Type[t.Any], *, unknown: str = "object") -> str:
    if issubclass(typ, str):
        return "string"
    elif hasattr(typ, "keys"):
        return "object"
    elif issubclass(typ, bool):
        return "boolean"
    elif issubclass(typ, int):
        return "integer"
    elif issubclass(typ, float):
        return "number"
    elif issubclass(typ, (list, tuple)):  # sequence?
        return "array"
    else:
        return unknown


def run(filename: str) -> None:
    import sys
    from pprint import pprint
    from magicalimport import import_module

    m = import_module(filename)
    builtins = set(sys.modules["builtins"].__dict__.keys())
    for k, v in m.__dict__.items():
        if k in builtins:
            continue
        if k.startswith("_"):
            continue
        if getattr(v, "__module__", None) != m.__name__:
            continue
        pprint(resolve_type_info(v))


def main(argv: t.Optional[t.List[str]] = None) -> None:
    import argparse

    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help  # type: ignore
    parser.add_argument("filename")
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == "__main__":
    main()
