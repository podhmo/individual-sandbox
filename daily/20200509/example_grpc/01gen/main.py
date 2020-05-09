from __future__ import annotations
import enum
import typing as t
from typestubs import uint32, uint
from typestubs import Date, Empty
from _emit import Service


class User:
    id: uint
    first_name: str
    family_name: str
    sex: Sex
    age: uint32
    birthday: Date


# https://en.wikipedia.org/wiki/ISO/IEC_5218 でなければautoが使える
@enum.unique
class Sex(enum.IntEnum):
    SEX_UNKNOWN = 0
    MALE = 1
    FEMALE = 2
    OTHER = 3  # 9


UserList = t.List[User]


class UserService(Service):
    def Get(self, req: GetRequest) -> User:
        pass

    def List(self, empty: Empty) -> UserList:
        pass


class GetRequest:
    id: uint


def main() -> None:
    import sys
    from _emit import scan_module
    from _emit import walk
    from _emit import emit

    module = sys.modules[__name__]
    defs = scan_module(module, is_ignored=lambda x: x == main)

    m = emit(walk(defs), name="myapp")
    print(m)


if __name__ == "__main__":
    main()
