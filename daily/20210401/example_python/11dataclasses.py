from dataclasses import dataclass, asdict


@dataclass
class Person:
    name: str

    @property
    def initial(self) -> str:
        return self.name.upper()[0]


p = Person(name="foo")

print(p)
# print(Person(name="foo", initial="F"))

print(asdict(p))
