import typing as t
import mypy_extensions as mx


class User(mx.TypedDict):
    id: str
    name: str
    age: int
    group_id: str


class Group(mx.TypedDict):
    id: str
    name: str


class X(mx.TypedDict):
    id: str


class Named(mx.TypedDict):
    name: str


def fetch_users() -> t.Sequence[User]:
    return []


def fetch_groups() -> t.Sequence[Group]:
    return []


def fetch_xs() -> t.Sequence[X]:
    return []


T = t.TypeVar("T", bound=Named)


def reject_empty(xs: t.Sequence[T]) -> t.Sequence[T]:
    return [x for x in xs if not x["name"]]


def reject_empty__with_typo(xs: t.Sequence[T]) -> t.Sequence[T]:
    # not error
    return [x for x in xs if not x["namee"]]


def main() -> None:
    for d0 in reject_empty(fetch_users()):
        print(d0["id"], d0["name"], d0["age"])

    # for d1 in reject_empty(fetch_groups()):
    #     print(d1["id"], d1["name"], d1["age"])

    for d1 in reject_empty(fetch_groups()):
        print(d1["id"], d1["name"])

    reject_empty(fetch_xs())
