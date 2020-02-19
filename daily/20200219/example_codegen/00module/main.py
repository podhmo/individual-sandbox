import typing as t
from common import User


class WUser(User):
    nickname: t.Optional[str] = None


def get_module(typ: t.Type[t.Any]) -> str:
    return getattr(typ, "__module__")


assert get_module(User) == "common"
assert get_module(WUser) == "__main__"

# accessing
from metashape.runtime import get_walker  # noqa 401

w = get_walker([WUser])
for cls in w.walk():
    for name, info, metadata in w.for_type(cls).walk():
        print(name, metadata)
