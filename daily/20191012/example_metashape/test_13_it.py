import typing as t
import typing_extensions as tx
import pytest
from magicalimport import import_symbol


_MyString = t.NewType("S", str)


class _Person:
    name: str


def atom(*, raw, underlying, is_optional=False, custom=None):
    return {
        "raw": raw,
        "underlying": underlying,
        "is_optional": is_optional,
        "custom": custom,
    }


def container(
    *, container, raw, is_optional=False, is_composite=False, raw_args=None, args=None
):
    raw_args = raw_args or [atom(raw=x, underlying=x) for x in args]
    return {
        "raw": raw,
        "args": tuple(raw_args),
        "container": container,
        "is_optional": is_optional,
        "is_composite": is_composite,
    }


@pytest.mark.parametrize(
    "typ, want",
    [
        (int, atom(raw=int, underlying=int)),
        (str, atom(raw=str, underlying=str)),
        (t.Optional[str], atom(raw=t.Optional[str], underlying=str, is_optional=True)),
        # container
        (t.List[str], container(raw=t.List[str], container="list", args=[str])),
        (
            t.Optional[t.List[str]],
            container(
                raw=t.Optional[t.List[str]],
                container="list",
                args=[str],
                is_optional=True,
            ),
        ),
        (
            t.List[t.Optional[str]],
            container(
                raw=t.List[t.Optional[str]],
                container="list",
                raw_args=[atom(raw=t.Optional[str], underlying=str, is_optional=True)],
            ),
        ),
        (list, container(raw=list, container="list", args=[t.Any])),
        (t.Tuple[str], container(raw=t.Tuple[str], container="tuple", args=[str])),
        (
            t.Tuple[str, int],
            container(raw=t.Tuple[str, int], container="tuple", args=[str, int]),
        ),
        (
            t.Dict[str, int],
            container(raw=t.Dict[str, int], container="dict", args=[str, int]),
        ),
        (
            t.Optional[t.Dict[str, int]],
            container(
                raw=t.Optional[t.Dict[str, int]],
                container="dict",
                args=[str, int],
                is_optional=True,
            ),
        ),
        (
            t.Optional[t.Dict[str, t.Optional[int]]],
            container(
                raw=t.Optional[t.Dict[str, t.Optional[int]]],
                container="dict",
                raw_args=[
                    atom(raw=str, underlying=str),
                    atom(raw=t.Optional[int], underlying=int, is_optional=True),
                ],
                is_optional=True,
            ),
        ),
        # schema
        (_Person, atom(raw=_Person, underlying=_Person, custom=_Person)),
        (
            t.Optional[_Person],
            atom(
                raw=t.Optional[_Person],
                underlying=_Person,
                custom=_Person,
                is_optional=True,
            ),
        ),
        (
            t.List[_Person],
            container(
                raw=t.List[_Person],
                container="list",
                raw_args=[atom(raw=_Person, underlying=_Person, custom=_Person)],
            ),
        ),
        # composite
        (
            t.Union[int, str],
            container(
                raw=t.Union[int, str],
                container="union",
                is_composite=True,
                raw_args=[atom(raw=int, underlying=int), atom(raw=str, underlying=str)],
            ),
        ),
        (
            # simplify -> t.Union[NoneType, _Person, str]
            t.Optional[t.Union[t.Optional[_Person], t.Optional[str]]],
            container(
                raw=t.Optional[t.Union[t.Optional[_Person], t.Optional[str]]],
                container="union",
                is_composite=True,
                is_optional=True,
                raw_args=[
                    atom(raw=_Person, underlying=_Person, custom=_Person),
                    atom(raw=str, underlying=str),
                ],
            ),
        ),
        # special
        (_MyString, atom(raw=_MyString, underlying=str)),
        (
            t.Optional[_MyString],
            atom(raw=t.Optional[_MyString], underlying=str, is_optional=True),
        ),
        (tx.Literal["A", "B"], atom(raw=tx.Literal["A", "B"], underlying=str)),
        (
            t.Optional[tx.Literal["A", "B"]],
            atom(
                raw=t.Optional[tx.Literal["A", "B"]], underlying=str, is_optional=True
            ),
        ),
    ],
)
def test_resolve_type_info(typ, want):
    callFUT = import_symbol("./13resolve.py:resolve_type_info")
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
    callFUT = import_symbol("./13resolve.py:resolve_type")
    got = callFUT(typ)
    assert got == want
