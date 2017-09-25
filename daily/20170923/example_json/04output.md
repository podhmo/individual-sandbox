``` python
from functools import singledispatch

inspector = None


def set_inspector(new_inspector):
    global inspector
    inspector = new_inspector


def get_inspector():
    return inspector


@singledispatch
def repr_inspector(o):
    return repr(o)


inspector = repr_inspector


@singledispatch
def vars_inspector(o):
    return str(vars(o))


class Model:
    def __str__(self):
        return inspector(self)


class Person(Model):
    def __init__(self, name, age):
        self.name = name
        self.age = age


print(Person("foo", 20))
set_inspector(vars_inspector)
print(Person("foo", 20))

# <Person object at 0x7f92e4e06a90>
# {'name': 'foo', 'age': 20}
```
