from functools import partial
from pprint import pformat


class DebugRepr:
    default_debug_repr = pformat

    def __init__(self, repr_fn):
        self.repr_fn = repr_fn
        self.debug_repr = None

    def __get__(self, ob, objtype=None):
        if ob is None:
            return self.repr_fn
        if self.debug_repr is None:
            return partial(self.repr_fn, ob)
        else:
            return partial(self.debug_repr, ob)

    def __set__(self, ob, debug_repr):
        if isinstance(debug_repr, bool):
            if debug_repr:
                debug_repr = self.__class__.default_debug_repr
            else:
                debug_repr = None
        self.debug_repr = debug_repr


default_registry = set()


def debug_repr(prop, registry=default_registry):
    if isinstance(prop, DebugRepr):
        return prop
    prop = DebugRepr(prop)
    registry.add(prop)
    return prop


def debug_all(name="__repr__"):
    for p in default_registry:
        setattr(p, name, True)


class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    @debug_repr
    def __str__(self):
        return self.name


p = Person("foo", 20)
print(str(p))
p.__str__ = True
print(str(p))
