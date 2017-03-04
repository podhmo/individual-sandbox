class Person(object):
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def __str__(self):
        return "{self.__class__.__name__}({self.name}, {self.age})".format(self=self)

d = {"name": "foo", "age": 20}
p = Person.__new__(Person)
p.__dict__.update(d)
print(p)
