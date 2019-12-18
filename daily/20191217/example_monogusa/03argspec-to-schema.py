from __future__ import annotations
import typing as t
import inspect
import dataclasses
from prestring.python import Module
from prestring.naming import pascalcase
from prestring.utils import reify


@dataclasses.dataclass
class FuncSpec:
    target_function: t.Callable[..., t.Any]
    argspec: inspect.FullArgSpec

    @reify
    def _classified(self) -> t.Dict[str, Kind]:
        return _classify_args(self.argspec)

    def kind_of(self, name: str) -> str:
        return self._classified[name]

    def type_str_of(self, typ: t.Type[t.Any]) -> str:
        return _resolve_type(typ)

    @reify
    def arguments(self) -> t.List[t.Tuple[str, t.Type[t.Any]]]:
        return [
            (name, v)
            for name, v in self.argspec.annotations.items()
            if name != "return"
        ]

    @reify
    def kwonlydefaults(self) -> t.Dict[str, t.Any]:
        return self.argspec.kwonlydefaults or {}


def func_spec(fn: t.Callable[..., t.Any]) -> FuncSpec:
    argspec = inspect.getfullargspec(do_something)
    annotations = t.get_type_hints(fn)
    assert len(argspec.annotations) == len(annotations)
    argspec.annotations.update(annotations)
    return FuncSpec(fn, argspec=argspec)


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


def argspec_to_schema(
    spec: FuncSpec, *, name: str, m: t.Optional[Module] = None
) -> str:
    if m is None:
        m = Module()
        m.from_("pydantic").import_("BaseModel")

    with m.class_(name, "BaseModel"):
        if len(spec.arguments) == 0:
            m.stmt("pass")

        for name, typ in spec.arguments:
            kind = spec.kind_of(name)
            if kind == "args":
                continue
            elif kind == "args_defaults":
                continue
            elif kind == "kw":
                m.stmt("{}: {}", name, spec.type_str_of(typ))
            elif kind == "kw_defaults":
                m.stmt(
                    "{}: {}  = {}",
                    name,
                    spec.type_str_of(typ),
                    spec.kwonlydefaults[name],
                )
            else:
                raise ValueError(f"invalid kind. name={name}, kind={kind}")
    return m


def do_something(dep: t.Any, x: int = 0, *, name: str, age: int = 0) -> None:
    pass


spec = func_spec(do_something)
print(argspec_to_schema(spec, name=pascalcase(do_something.__name__)))
