import typing as t
from egoist.generate.go.types import GoError


class Config:
    gopackage = "m/config"


def NewConfig(filename: str) -> t.Tuple[Config, GoError]:
    pass
