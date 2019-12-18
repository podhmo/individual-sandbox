import typing as t
import inspect
from prestring.python import Module
from prestring.naming import pascalcase


def get_full_argspec(fn: t.Callable[..., t.Any]) -> inspect.FullArgSpec:
    argspec = inspect.getfullargspec(do_something)
    annotations = t.get_type_hints(fn)
    assert len(argspec.annotations) == len(annotations)
    argspec.annotations.update(annotations)
    return argspec


def _classify_args(spec: inspect.FullArgSpec) -> t.Dict[str, str]:
    classified = {}
    args_limit = len(spec.args) - len(spec.defaults)
    for i in range(args_limit):
        classified[spec.args[i]] = "args"
    for i in range(1, len(spec.defaults) + 1):
        classified[spec.args[-i]] = "args_defaults"
    for k in spec.kwonlyargs:
        classified[k] = "kw"
    for k in spec.kwonlydefaults:
        classified[k] = "kw_defaults"
    return classified


def _resolve_type(typ: t.Type[t.Any]) -> None:
    # TODO: import?
    return typ.__name__


def argspec_to_schema(
    spec: inspect.FullArgSpec, classname: str, *, m: t.Optional[Module] = None
) -> str:
    if m is None:
        m = Module()
        m.from_("pydantic").import_("BaseModel")

    classified = _classify_args(spec)

    with m.class_(classname, "BaseModel"):
        fields = [(name, v) for name, v in spec.annotations.items() if name != "return"]
        if len(fields) == 0:
            m.stmt("pass")
        for name, typ in fields:
            kind = classified[name]
            if kind == "args":
                continue
            elif kind == "args_defaults":
                continue
            elif kind == "kw":
                m.stmt("{}: {}", name, _resolve_type(typ))
            elif kind == "kw_defaults":
                m.stmt(
                    "{}: {}  = {}", name, _resolve_type(typ), spec.kwonlydefaults[name]
                )
            else:
                raise ValueError(f"invalid kind. name={name}, kind={kind}")
    return m


def do_something(dep: t.Any, x: int = 0, *, name: str, age: int = 0) -> None:
    pass


argspec = get_full_argspec(do_something)
print(argspec)
print("----------------------------------------")
print(pascalcase(do_something.__name__))
print("----------------------------------------")
print(argspec_to_schema(argspec, classname=pascalcase(do_something.__name__)))
