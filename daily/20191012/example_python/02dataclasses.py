import dataclasses


@dataclasses.dataclass
class P:
    x: int
    y: int = 0


print(P(x=10))
# P(x=10, y=0)
