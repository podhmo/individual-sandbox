import traitlets


class Person(traitlets.HasTraits):
    name = traitlets.Any(default_value="foo")
    age = traitlets.Int(default_value=0)

    parent = traitlets.Instance("__main__.Person", allow_none=True)

    @traitlets.validate("age")
    def _valid_age(self, proposal):
        age = proposal["value"]
        if age < 0:
            raise traitlets.TraitError("must be positive")
        return proposal["value"]


p = Person()
print(p.name, p.age)

try:
    p = Person(age=-10)
except traitlets.TraitError as e:
    print("!", repr(e))

try:
    p = Person(parent=10)
except traitlets.TraitError as e:
    print("!!", repr(e))

p = Person(parent=Person())
print(p.name, p.age, p.parent)

# foo 0
# ! TraitError('must be positive',)
# !! TraitError("The 'parent' trait of a Person instance must be a Person or None, but a value of class 'int' (i.e. 10) was specified.",)
# foo 0 <__main__.Person object at 0x7fcd6135a6d8>
