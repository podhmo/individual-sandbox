class AMeta(type):
    def __new__(self, clsname, bases, attrs, name=None):
        instance = super().__new__(self, clsname, bases, attrs)
        if "children" not in instance.__dict__:
            instance.children.add(name or clsname)
        return instance


class A(metaclass=AMeta):
    children = set()


class B(A):
    pass


class C(A, name="MaybeA"):
    pass


print(A.children)
