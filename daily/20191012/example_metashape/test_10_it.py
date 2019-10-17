import typing as t
import pytest
from magicalimport import import_symbol


class _Person:
    name: str


atom = import_symbol("./10resolve.py:Atom")
container = import_symbol("./10resolve.py:Container")


@pytest.mark.parametrize(
    "typ, want",
    [
        (int, atom(raw=int, base=int, args=(int, ))),
        # (str, atom(raw=str, base=str, args=[str])),
        # (
        #     t.Optional[str],
        #     atom(raw=t.Optional[str], base=str, args=[str], is_optional=True),
        # ),
        # (t.List[str], container(raw=t.List[str], container=list, items=[str])),
        # (
        #     t.Optional[t.List[str]],
        #     container(
        #         raw=t.Optional[t.List[str]],
        #         container=list,
        #         items=[str],
        #         is_optional=True,
        #     ),
        # ),
        # (
        #     t.List[t.Optional[str]],
        #     container(
        #         raw=t.List[t.Optional[str]],
        #         container=list,
        #         raw_items=[
        #             atom(raw=t.Optional[str], base=str, args=[str], is_optional=True)
        #         ],
        #     ),
        # ),
        # (list, container(raw=list, container=list, items=[t.Any])),
    ],
)
def test_resolve_type_info(typ, want):
    callFUT = import_symbol("./10resolve.py:resolve_type_info")
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
    callFUT = import_symbol("./10resolve.py:resolve_type")
    got = callFUT(typ)
    assert got == want
