import inspect
import typing as t


class Foo:
    pass


def f() -> Foo:
    pass


def g() -> "Foo":
    pass


spec = inspect.getfullargspec(f)
print(spec.annotations["return"])

spec = inspect.getfullargspec(g)
spec.annotations.update(t.get_type_hints(g))
print(spec.annotations["return"])

spec = inspect.getfullargspec(g)
globalns = g.__globals__
localns = globalns
print(t._eval_type(t.ForwardRef(spec.annotations["return"]), globalns, localns))
