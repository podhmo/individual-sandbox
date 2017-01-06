import unittest.mock as mock


class Person(object):
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def greeting(self):
        return "hello"


m = mock.Mock(spec=Person)
for name, value in sorted(m.__dict__.items()):
    if name.startswith("_spec"):
        print(name, value)

# _spec_class <class '__main__.Person'>
# _spec_set True
# _spec_signature (name, age)

m.name = "bar"
print(m.name)

print(m.greeting())
try:
    print(m.greeting2())
except AttributeError as e:
    print(e)

m = mock.Mock(spec_set=Person)
m.name = "boo"  # error (spec_set=Trueなので)
