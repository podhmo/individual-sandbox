import typing as t
import logging
from metashape import runtime


class User:
    name: str


UserList = t.List[User]
X = t.NewType("X", str)
XList = t.List[X]
logging.basicConfig(level=logging.DEBUG)
MyUser = t.NewType("MyUser", User)
MyUserList = t.List[MyUser]
w = runtime.get_walker({"UserList": UserList, "XList": XList, "MyUserList": MyUserList})
for cls in w.walk():
    print(cls, w.resolver.resolve_typename(cls))
