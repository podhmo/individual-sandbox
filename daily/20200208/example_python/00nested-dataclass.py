import dataclasses


@dataclasses.dataclass
class B:
    name: str = "B"


@dataclasses.dataclass
class A:
    name: str = "A"
    b: B = dataclasses.field(default_factory=B)


print(A())
print(dataclasses.asdict(A()))
