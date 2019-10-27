import typing as t
import typing_extensions as tx


def is_member_aggressive(cls: t.Type) -> bool:
    return (
        hasattr(cls, "__name__")
        and not hasattr(cls, "__loader__")
        and (
            hasattr(cls, "__annotations__")  # for custom
            or (
                hasattr(cls, "__origin__")
                and cls.__origin__ is tx.Literal  # for enum (new type)
            )
        )
    )


class Zero:
    pass


class Person:
    name: str


Kind = tx.Literal["story", "epic", "task"]

print(Zero, is_member_aggressive(Zero))  # <class '__main__.Zero'> False
print(Person, is_member_aggressive(Person))  # <class '__main__.Person'> True
print(
    Kind, is_member_aggressive(Kind)
)  # typing_extensions.Literal['story', 'epic', 'task'] False
# print(vars(Kind))
