from __future__ import annotations
import typing as t
import inspect
import dataclasses
from functools import update_wrapper
from prestring.utils import reify


@dataclasses.dataclass
class FuncSpec:
    target_function: t.Callable[..., t.Any]
    argspec: inspect.FullArgSpec

    @property
    def name(self):
        return self.target_function.__name__

    @property
    def fullname(self):
        return f"{self.target_function.__module__}.{self.target_function.__name__}"

    @property
    def doc(self):
        return self.target_function.__doc__

    def kind_of(self, name: str) -> str:
        return self._classified[name]

    def default_of(self, name: str) -> str:
        return self._kwonlydefaults[name]

    def type_str_of(self, typ: t.Type[t.Any]) -> str:
        return _resolve_type(typ)

    @reify
    def parameters(self) -> t.List[t.Tuple[str, t.Type[t.Any]]]:
        return [
            (name, v)
            for name, v in self.argspec.annotations.items()
            if name != "return"
        ]

    @reify
    def _classified(self) -> t.Dict[str, Kind]:
        return _classify_args(self.argspec)

    @reify
    def _kwonlydefaults(self) -> t.Dict[str, t.Any]:
        return self.argspec.kwonlydefaults or {}


def fnspec(fn: t.Callable[..., t.Any]) -> FuncSpec:
    argspec = inspect.getfullargspec(fn)
    annotations = t.get_type_hints(fn)
    assert len(argspec.annotations) == len(annotations)
    argspec.annotations.update(annotations)
    spec = FuncSpec(fn, argspec=argspec)
    update_wrapper(spec, fn)  # type: ignore
    return spec


Kind = t.Literal["args", "args_defaults", "kw", "kw_defaults"]


def _classify_args(spec: inspect.FullArgSpec) -> t.Dict[str, Kind]:
    classified: t.Dict[str, Kind] = {}
    args_limit = len(spec.args or []) - len(spec.defaults or [])
    for i in range(args_limit):
        classified[spec.args[i]] = "args"
    for i in range(1, len(spec.defaults or []) + 1):
        classified[spec.args[-i]] = "args_defaults"
    for k in spec.kwonlyargs:
        classified[k] = "kw"
    for k in spec.kwonlydefaults or []:
        classified[k] = "kw_defaults"
    return classified


def _resolve_type(typ: t.Type[t.Any]) -> str:
    # TODO: import?
    return typ.__name__
