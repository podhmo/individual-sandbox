import operator
from prestring.utils import LazyFormat


# https://docs.python.org/ja/3/library/operator.html


class Symbol:
    def __eq__(self, x):
        return Symbol(str(LazyFormat("{} == {}", self, x)))

    def __ne__(self, x):
        return Symbol(str(LazyFormat("{} != {}", self, x)))

    def __gt__(self, x):
        return Symbol(str(LazyFormat("{} < {}", self, x)))


print(operator.is_(True, True))
print(operator.not_(True))
foo = Symbol("foo")
print(foo)
print((foo == 1))
print((foo != 1))
