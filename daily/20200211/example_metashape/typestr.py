import sys
import typing as t
import typing_extensions as tx
import unittest


# TODO: import callback
# TODO: memoize


def typestr(typ: t.Optional[t.Type[t.Any]], aliases=None, _nonetype=type(None)) -> str:
    if typ is None:
        return "None"
    if typ == ...:
        return "..."

    if typ.__module__ in ("builtins", "__main__"):
        if typ is _nonetype:
            return "None"
        prefix = ""
    elif aliases:
        prefix = f"{aliases.get(typ.__module__, typ.__module__)}."
    else:
        prefix = f"{typ.__module__}."

    if hasattr(typ, "__qualname__"):
        return f"{prefix}{typ.__qualname__}"
    elif hasattr(typ, "__origin__"):  # for typing.Union, typing.Optional, ...
        args = typ.__args__
        origin = typ.__origin__

        # e.g. Optional[int]
        if origin == t.Union and len(args) == 2:
            args = [x for x in args if x is not _nonetype]
            if len(args) == 1:
                return f"{prefix}Optional[{typestr(args[0], aliases=aliases)}]"
        elif origin == t.collections.abc.Callable:
            size = len(args)
            if size == 0:
                # e.g. t.Callable
                return f"{prefix}Callable[..., {prefix}Any]"
            elif size == 1:
                # e.g. t.Callable[[], None]
                return f"{prefix}Callable[[], {typestr(args[0], aliases=aliases)}]"
            elif size == 2 and args[0] == ...:
                # e.g. t.Callable[..., t.Any]
                return f"{prefix}Callable[..., {typestr(args[1], aliases=aliases)}]"
            else:
                # e.g. t.Callable[[int, int], int]
                return f"{prefix}Callable[[{', '.join(typestr(x, aliases) for x in args[:-1])}], {typestr(args[-1], aliases=aliases)}]"
        elif origin == tx.Literal:
            # e.g. tx.Literal[False]
            pass
        else:
            # e.g. t.List[str]
            name = getattr(typ, "_name") or getattr(origin, "_name")
            return f"{prefix}{name}[{', '.join(typestr(x, aliases=aliases) for x in args)}]"
    elif str(typ).startswith(("~", "+", "-")):
        if typ.__module__ == "typing":
            # e.g. t.List
            return f"{prefix}Any"
        else:
            # e.g. t.List[A]
            return f"{prefix}{typ.__name__}"

    # e.g. typing.List[typing.Any]
    # e.g. T = typing.TypeVar("T"); typing.List[T]
    return str(typ).replace(f"{typ.__module__}.", prefix).replace("~", "")


class Tests(unittest.TestCase):
    def _callFUT(self, typ, aliases=None):
        return typestr(typ, aliases=aliases)

    def test_primitives(self):
        # fmt:off
        cases = [
            (bool, "bool"),
            (bytearray, "bytearray"),
            (bytes, "bytes"),
            (complex, "complex"),
            (dict, "dict"),
            (float, "float"),
            (frozenset, "frozenset"),
            (int, "int"),
            (list, "list"),
            (object, "object"),
            (set, "set"),
            (str, "str"),
            (tuple, "tuple"),
            (None, "None"),
        ]
        # fmt:on
        for cls, want in cases:
            with self.subTest(from_=cls, to=want):
                got = self._callFUT(cls)
                self.assertEqual(got, want)

    def test_imported(self):
        from collections import Counter

        # fmt:off
        cases = [
            (Counter, "collections.Counter", {}),
            (Counter, "c.Counter", {"collections": "c"}),
        ]
        # fmt:on

        for cls, want, aliases in cases:
            with self.subTest(from_=cls, to=want, aliases=aliases):
                got = self._callFUT(cls, aliases=aliases)
                self.assertEqual(got, want)

    def test_typing(self):
        from typing import List, Dict, Any, Union, Optional, TypeVar, Callable
        from typing_extensions import Literal

        A = TypeVar("A")

        aliases = {"typing": "t", "typing_extensions": "tx"}
        # fmt:off
        cases = [
            (List, "t.List[t.Any]"),
            (List[Any], "t.List[t.Any]"),
            (List[str], "t.List[str]"),
            (List[A], "t.List[A]"),  # xxx
            (Union[int, str], "t.Union[int, str]"),
            (Union[int, None], "t.Optional[int]"),
            (Union[None, int], "t.Optional[int]"),
            (Optional[int], "t.Optional[int]"),
            (Callable[[], None], "t.Callable[[], None]"),
            (Callable[[], Any], "t.Callable[[], t.Any]"),
            (Callable[..., Any], "t.Callable[..., t.Any]"),
            (Union[Optional[Dict[str, Dict[str, str]]], Any],
             "t.Union[t.Dict[str, t.Dict[str, str]], None, t.Any]"),
            (Literal[False], ("t.Literal[False]" if sys.version_info[:2] >= (3, 8) else "tx.Literal[False]"))
        ]
        # fmt:on

        for cls, want in cases:
            with self.subTest(from_=cls, to=want):
                got = self._callFUT(cls, aliases=aliases)
                self.assertEqual(got, want)


if __name__ == "__main__":
    unittest.main()
