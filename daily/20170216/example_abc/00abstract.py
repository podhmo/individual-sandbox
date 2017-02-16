import abc


class Base(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def foo(self):
        pass


class A(Base):
    pass


class B(Base):
    def foo(self):
        pass

try:
    print(A())  # error
except TypeError as e:
    print(e)

print(B())
