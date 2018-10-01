import typing as t
import mypy_extensions as mx


class User(mx.TypedDict):
    id: str
    group_id: str
    name: str


class Group(mx.TypedDict):
    id: str
    name: str


Target = t.Union[User, Group]
d0: Group = {"name": "foo", "id": "id"}
d1: Target = t.cast(Target, {"name": "foo", "id": "id"})
d2: t.Optional[Group] = {"name": "foo", "id": "id"}
_d4: Group = {"name": "foo", "id": "id"}
d4: Target = _d4
