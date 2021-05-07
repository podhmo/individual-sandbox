from dataclasses import dataclass, field


@dataclass
class Named:
    full_name: str


@dataclass
class Person(Named):
    full_name: str = "XXX YYY"
    first_name: str = field(init=False)
    last_name: str = field(init=False)

    def __post_init__(self):
        self.first_name = self.full_name.split(" ", 2)[0]
        self.last_name = self.full_name.split(" ", 2)[0]


p = Person(full_name="Foo Bar")
print(p)
# Person(full_name='Foo Bar', first_name='Foo', last_name='Foo')
p = Person()
print(p)
