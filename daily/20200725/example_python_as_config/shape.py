from __future__ import annotations
import typing as t
from pydantic.dataclasses import dataclass
from dataclasses import field
import k8shape


@dataclass
class Config:
    spec: ConfigSpec
    apiVersion: str = field(default="apps/v1")
    kind: str = field(default="Development")
    metadata: t.Dict[str, str] = field(default_factory=dict)


@dataclass
class ConfigSpec:
    replicas: int
    template: ConfigTemplate


@dataclass
class ConfigTemplate:
    spec: ConfigTemplateSpec
    metadata: t.Dict[str, t.Dict[str, str]] = field(default_factory=dict)


@dataclass
class ConfigTemplateSpec:
    containers: t.List[k8shape.Container]


ConfigTemplateSpec.__pydantic_model__.update_forward_refs()
ConfigTemplate.__pydantic_model__.update_forward_refs()
ConfigSpec.__pydantic_model__.update_forward_refs()
Config.__pydantic_model__.update_forward_refs()
