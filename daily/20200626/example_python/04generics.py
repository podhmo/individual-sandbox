import typing as t


T = t.TypeVar("T")


class Query(t.Generic[T]):
    @classmethod
    def __emit__(cls, name):
        print("emit", name)


s = Query[str]
print(s)
print(type(s))
print(vars(s))
