from abc import ABCMeta, abstractmethod


class N(type):
    def __init__(self, name, bases, attrs):
        return None


class X(metaclass=N):
    def __new__(self):
        return None

print(X())


class MyMeta(ABCMeta):
    def __call__(self, *args, **kwargs):
        print("C", self, args, kwargs)
        return super().__call__(*args, **kwargs)


class C(metaclass=MyMeta):
    @abstractmethod
    def m(self):
        pass


class CC(C):
    pass


CC()
