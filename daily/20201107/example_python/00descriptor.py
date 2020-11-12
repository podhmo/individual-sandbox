class Upper:
    def __set_name__(self, cls, name):
        self.name = "_" + name

    def __get__(self, ob, cls):
        v = getattr(ob, self.name)
        return v.upper()


class X:
    name: str = Upper()

    def __init__(self, *, name: str = "") -> None:
        self._name = name


x = X(name="foo")
print(vars(x), x.name)
