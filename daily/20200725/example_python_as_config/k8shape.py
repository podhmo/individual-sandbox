from __future__ import annotations
import typing as t
from pydantic.dataclasses import dataclass


@dataclass
class Container:
    name: str
    image: str
    ports: t.List[PortSetting]


@dataclass
class PortSetting:
    containerPort: int


PortSetting.__pydantic_model__.update_forward_refs()
Container.__pydantic_model__.update_forward_refs()
