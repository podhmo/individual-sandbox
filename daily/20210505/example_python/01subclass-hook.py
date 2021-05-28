class A:
    def __init_subclass__(cls, *, name: str = "", **kwargs):
        if not name:
            name = cls.__name__
        print(cls, name)


class B(A):
    pass


class C(A, name="c"):
    pass
