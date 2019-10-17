import typing as t
import typing_extensions as tx
import typing_inspect

TypeInfo = t.Union["TypeInfoAtom", "TypeInfoContainer"]


class TypeInfoContainer(tx.TypedDict, total=True):
    raw: t.Type[t.Any]  # t.Optional[t.List[int]] -> t.Optional[t.List[int]]
    container: t.Type[t.Any]
    items: t.Tuple[TypeInfo]

    is_optional: bool  # t.Optional[int] -> True, int -> False
    is_composite: bool  # t.Union -> True, dict -> False


class TypeInfoAtom(tx.TypedDict, total=True):
    raw: t.Type[t.Any]  # t.Optional[t.List[int]] -> t.Optional[t.List[int]]
    args: t.Sequence[t.Type[t.Any]]
    base: t.Type[t.Any]  # t.Optionall[int] -> int, t.Dict[str, t.Any] -> dict

    is_optional: bool  # t.Optional[int] -> True, int -> False
    is_composite: bool  # t.Union -> True, dict -> False
    custom: t.Optional[
        t.Type[t.Any]
    ]  # int -> None, Person -> Person, t.List[int] -> None


def _make_atom(*, raw, base, args, is_optional=False, custom=None) -> TypeInfoAtom:
    return {
        "raw": raw,
        "base": base,
        "args": tuple(args),
        "is_optional": is_optional,
        "is_composite": False,
        "custom": custom,
    }


def _make_container(*, container, raw, items, is_optional=False) -> TypeInfoContainer:
    return {
        "raw": raw,
        "items": tuple(items),
        "container": container,
        "is_optional": is_optional,
        "is_composite": False,
    }


def resolve_type_info(
    typ: t.Type[t.Any],
    *,
    is_optional=False,
    custom=None,
    _nonetype=type(None),
    _primitives=set([str, int, bool, str, bytes, dict, list, t.Any]),
) -> TypeInfo:
    raw = typ
    args = typing_inspect.get_args(typ)
    base = getattr(typ, "__origin__", None)
    if base is None:
        if not hasattr(typ, "__iter__"):
            base = typ  # xxx
        elif issubclass(typ, str):
            base = typ
        elif issubclass(typ, t.Sequence):
            return _make_container(
                raw=raw, container=list, items=(resolve_type_info(t.Any),)
            )
        elif issubclass(typ, t.Mapping):
            return _make_container(
                raw=raw,
                container=dict,
                items=(resolve_type_info(t.Any), resolve_type_info(t.Any)),
            )
        else:
            base = typ  # xxx
    else:
        if len(args) == 2 and base == t.Union:
            is_optional = True
            if args[0] == _nonetype:
                base = args[1]
                args = (args[1],)
            elif args[1] == _nonetype:
                base = args[0]
                args = (args[0],)
            typ = args[0]
        if hasattr(typ, "__origin__"):
            base = typ.__origin__
            if issubclass(base, t.Sequence):
                args = typing_inspect.get_args(typ)
                # TODO: tuple
                return _make_container(
                    raw=raw,
                    container=list,
                    items=tuple([resolve_type_info(t) for t in args]),
                    is_optional=is_optional,
                )
            elif issubclass(base, t.Mapping):
                args = typing_inspect.get_args(typ)
                return _make_container(
                    raw=raw,
                    container=dict,
                    items=tuple([resolve_type_info(t) for t in args]),
                    is_optional=is_optional,
                )
            else:
                raise ValueError(f"unsuported type %{typ}")
    if not args:
        args = (base,)
    if base not in _primitives:
        custom = base
    return _make_atom(
        raw=raw, args=args, base=base, is_optional=is_optional, custom=custom
    )


def resolve_type(origin: t.Type[t.Any], *, unknown="object") -> str:
    if issubclass(origin, str):
        return "string"
    elif hasattr(origin, "keys"):
        return "object"
    elif issubclass(origin, bool):
        return "boolean"
    elif issubclass(origin, int):
        return "integer"
    elif issubclass(origin, float):
        return "number"
    elif issubclass(origin, (list, tuple)):  # sequence?
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


def main(argv=None):
    import argparse

    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument("filename")
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == "__main__":
    main()
