class A:
    pass

print(A().__class__)
print(A.__class__)


class MyMeta(type):
    def __new__(cls, name, bases, attrs, **kwargs):
        # class My:
        #     pass
        # return My
        print("N", cls, name, bases, attrs, kwargs)
        return super().__new__(cls, name, bases, attrs)

    def __init__(self, name, bases, attrs, **kwargs):
        print("I", self, name, bases, attrs, kwargs)
        return super().__init__(name, bases, attrs)

    def __call__(self, *args, **kwargs):
        print("C", self, args, kwargs)
        return super().__call__(*args, **kwargs)


class B(metaclass=MyMeta, foo="bar"):
    def b(self):
        return "b"

b = B()
print(b.__class__)
print(b.b())
