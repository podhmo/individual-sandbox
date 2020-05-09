import typing as t
import sys
from typestubs import User, UserList

UserList2 = t.List[User]

for name, val in list(sys.modules[__name__].__dict__.items()):
    if name.startswith("_"):
        continue
    if not hasattr(val, "__module__"):
        continue

    if not (val.__module__ != __name__ or hasattr(val, "__origin__")):
        continue
    print(name, val)
