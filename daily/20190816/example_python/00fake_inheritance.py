class A:
    pass


class B:
    def __init__(self):
        self.__class__ = A


print(isinstance(B(), A))
print(issubclass(B, A))
