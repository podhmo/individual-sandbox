import traitlets


class Person(traitlets.HasTraits):
    name = traitlets.Any(default_value="foo")
    age = traitlets.Int(default_value=0)

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

# foo 0
# ! TraitError('must be positive',)
