from monogusa import default_component
from monogusa.dependencies import component, resolve_args


class Foo:
    pass


@default_component
def foo() -> Foo:
    return Foo()


def fn(f: Foo) -> None:
    print(f)


print(vars(component))
resolve_args(fn)
