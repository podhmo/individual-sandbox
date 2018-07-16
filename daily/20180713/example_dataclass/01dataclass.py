from dataclasses import dataclass


@dataclass(frozen=True)
class Option:
    x: bool = False
    y: bool = False


print(Option())
print(Option(y=True))
