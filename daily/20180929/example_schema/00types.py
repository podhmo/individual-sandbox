import typing as t


class Person:
    name: str
    age: int

    def __init__(self, name: str, age: int) -> None:
        self.name = name
        self.age = age


print(Person.__annotations__)
# {'name': <class 'str'>, 'age': <class 'int'>}
print(t.get_type_hints(Person))
# {'name': <class 'str'>, 'age': <class 'int'>}
