class MZero:
    def __repr__(self):
        return "-"

    def __add__(self, n):
        return n

    def __sub__(self, n):
        return 0 - n

    def __rsub__(self, n):
        return n

    def __div__(self, n):
        return self

    def __rdiv__(self, n):
        return 0 / n

    __truediv__ = __floordiv__ = __div__
    __rtruediv__ = __rfloordiv__ = __rdiv__

    def __bool__(self):
        return False


print(MZero())
print(bool(MZero()))
print(MZero() + 10)
print(MZero() - 10)
print(10 - MZero())
d = {"c": MZero()}
d["c"] += 1
print(d)
print("[]", MZero() / 1)
print(1 / MZero())
