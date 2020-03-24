import abc


class Iface(abc.ABC):
    pass


class A:
    pass


class B:
    pass


Iface.register(A)
Iface.register(B)
print(issubclass(A, Iface))
print(issubclass(B, Iface))
