import dataclasses


class Upper:
    def __init__(self, name=None):
        self.name = name

    def __set_name__(self, cls, name):
        if self.name is None:
            self.name = "_" + name

    def __get__(self, ob, cls):
        v = getattr(ob, self.name)
        return v.upper()


@dataclasses.dataclass
class X:
    name: str

    @property
    def upper_name(self) -> str:
        return self.name.upper()


x = X(name="foo")
print(vars(x), x.name, x.upper_name)
