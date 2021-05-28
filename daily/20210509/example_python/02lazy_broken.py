from __future__ import annotations
import weakref
import typing as t
import dataclasses

T = t.TypeVar("T")


class Undefined:
    def __repr__(self) -> str:
        return "<UNDEFINED>"


UNDEFINED = Undefined()


POOL = weakref.WeakKeyDictionary()


class Prop(t.Generic[T]):
    def __init__(self, *, name: t.Optional[str] = None) -> None:
        self.name = name
        self.store_name = "_"

    def __repr__(self) -> str:
        cls = self.__class__
        return f"<{cls.__module__}.{cls.__name__} object at {hex(id(self))}, field={self.name!r}>"

    def __get__(  # noqa:F811
        self, ob: t.Optional[t.Any], typ: t.Optional[t.Type[t.Any]] = None
    ) -> t.Union[T, Undefined]:
        if ob is None:
            return self

        if hasattr(ob, self.store_name):
            return getattr(ob, self.store_name)

        print("@", ob)
        v = POOL.get(ob)
        if v is None:
            v = POOL[ob] = []
        v.append(self.name)

        return UNDEFINED

    def __set__(self, ob: t.Any, value: T) -> None:
        # v = POOL.get(ob)
        # v.remove(self.name)
        setattr(ob, self.store_name, value)

    def __set_name__(self, ob: t.Any, name: str) -> None:
        self.name = name
        self.store_name = f"_{name}"


@dataclasses.dataclass
class A:
    name: str = Prop[str]()


@dataclasses.dataclass
class B:
    name: str = Prop[str]()
    a_name: str = Prop[str]()


def run(a):
    b = B(a_name=a.name)
    # 評価器が辛くなる？
    return (a, b)


# ホスティング的なことが必要？
# dataclassesとdescriptorは相性が悪い

# print(run(A(name="foo")))
# print(run(A()))

print(run(A()))
print("----------------------------------------")
print(POOL)
