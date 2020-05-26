import typing as t


class X:
    pass


class Y:
    pass


XOrYList = t.List[t.Union[X, Y]]
XOrYList2 = t.Union[t.List[X], t.List[Y]]
print(XOrYList, XOrYList2)
