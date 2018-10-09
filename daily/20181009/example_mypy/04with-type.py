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
    upcasted: t.Sequence[Named] = xs
    r = [x for x in upcasted if not x["name"]]
    return t.cast(t.Sequence[T], r)


def reject_empty__with_typo(xs: t.Sequence[T]) -> t.Sequence[T]:
    upcasted: t.Sequence[Named] = xs
    r = [x for x in upcasted if not x["namee"]]
    return t.cast(t.Sequence[T], r)


def main() -> None:
    # reveal_type(User)
    # reveal_type(type(fetch_users()[0]))

    for d0 in reject_empty__with_typo(fetch_users()):
        # reveal_type(d0)
        print(d0["id"], d0["name"], d0["age"])
    for d1 in reject_empty__with_typo(fetch_groups()):
        print(d1["id"], d1["name"])

    # error
    reject_empty__with_typo(fetch_xs())
