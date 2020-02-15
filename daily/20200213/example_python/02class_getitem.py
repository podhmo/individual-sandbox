import typing_extensions as tx


class I(tx.Protocol):
    pass


class A:
    def __class_getitem__(self, k):
        return I


print(A["A"])
