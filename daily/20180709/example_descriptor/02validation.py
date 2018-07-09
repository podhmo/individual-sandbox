class validation:
    def __init__(self, prop):
        self.prop = prop

    def __get__(self, ob, typ=None):
        if ob is None:
            return self.prop
        val = ob.__dict__[self.prop.__name__]
        return self.prop(ob, val)

    def __set__(self, ob, val):
        if ob is None:
            raise RuntimeError("can't modified")
        ob.__dict__[self.prop.__name__] = val


class Person:
    def __init__(self, name, age=0):
        self.name = name
        self.age = age

    @validation
    def age(self, value):
        if value < 0:
            raise ValueError("must be positive value")
        return value

    def decrease(self, n):
        self.age = -n


p = Person("foo", 20)
print(p.name, p.age)
print(p.__dict__["age"])
print("----------------------------------------")
p.decrease(20)
print(p.name, p.age)
