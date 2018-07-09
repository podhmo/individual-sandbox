from functools import partial


class Person:
    def __init__(self, name, age=0):
        self.name = name
        self._age = age

    @property
    def age(self):
        value = self._age
        if value < 0:
            raise ValueError("must be positive value")
        return value

    def decrease(self, n):
        self._age = -n


p = Person("foo", 20)
print(p.name, p.age)

print("----------------------------------------")
try:
    p.decrease(20)
    print(p.name, p.age)
except Exception as e:
    print("!", e)
