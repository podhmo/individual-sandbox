from collections import namedtuple


class Option(namedtuple("Option", "x, y")):
    def __new__(self, x=False, y=False):
        return super().__new__(self, x=x, y=y)


print(Option())
print(Option(y=True))
