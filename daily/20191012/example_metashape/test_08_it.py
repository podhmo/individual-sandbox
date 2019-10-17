import typing as t
import pytest
from magicalimport import import_symbol


class _Person:
    name: str


@pytest.mark.parametrize(
    "typ, want",
    [
        (
            int,
            {
                "raw": int,
                "args": (int,),
                "base": int,
                "container": None,
                "is_optional": False,
                "is_composite": False,
                "is_primitive": True,
            },
        ),
        (
            str,
            {
                "raw": str,
                "args": (str,),
                "base": str,
                "container": None,
                "is_optional": False,
                "is_composite": False,
                "is_primitive": True,
            },
        ),
        (
            t.Optional[str],
            {
                "raw": t.Optional[str],
                "args": (str, type(None)),
                "base": str,
                "container": None,
                "is_optional": True,
                "is_composite": False,
                "is_primitive": True,
            },
        ),
        (
            t.List[str],
            {
                "raw": t.List[str],
                "args": (str,),
                "base": str,
                "container": list,
                "is_optional": False,
                "is_composite": False,
                "is_primitive": True,
            },
        ),
        (
            t.Optional[t.List[str]],
            {
                "raw": t.Optional[t.List[str]],
                "args": (str,),
                "base": str,
                "container": list,
                "is_optional": True,
                "is_composite": False,
                "is_primitive": True,
            },
        ),
        (
            t.List[t.Optional[str]],
            {
                "raw": t.List[t.Optional[str]],
                "args": (t.Optional[str],),
                "base": t.Optional[str],
                "container": list,
                "is_optional": False,
                "is_composite": False,
                "is_primitive": True,
            },
        ),
        (
            list,
            {
                "raw": list,
                "args": (t.Any,),
                "base": t.Any,
                "container": list,
                "is_optional": False,
                "is_composite": False,
                "is_primitive": True,
            },
        ),
    ],
)
def test_resolve_type_info(typ, want):
    callFUT = import_symbol("./08resolve.py:resolve_type_info")
    got = callFUT(typ)
    assert got == want


@pytest.mark.parametrize(
    "typ, want",
    [
        (int, "integer"),
        (float, "number"),
        (str, "string"),
        (bool, "boolean"),
        (_Person, "object"),
        (dict, "object"),
        (list, "array"),
    ],
)
def test_resolve_type(typ, want):
    callFUT = import_symbol("./08resolve.py:resolve_type")
    got = callFUT(typ)
    assert got == want
