class A:
    children = set()

    def __init_subclass__(cls, name=None):
        name = name or cls.__name__
        cls.children.add(name)


class B(A):
    pass


class C(A, name="MaybeA"):
    pass


print(A.children)
