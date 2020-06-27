from __future__ import annotations
import typing as t
from collections import defaultdict
from functools import lru_cache, partial
from collections import deque
import re
import runtime
from egoist.internal._fnspec import fnspec, Fnspec


if t.TYPE_CHECKING:
    from metashape.analyze.walker import Walker

_nonetype = type(None)


def get_walker(fns: t.List[t.Callable[..., t.Any]]) -> Walker:
    from metashape.runtime import get_walker as _get_walker
    from metashape.analyze.config import Config
    from egoist.internal._fnspec import fnspec

    dq = deque(fns)

    seen: t.Set[t.Type[t.Any]] = set()
    for fn in fns:
        spec = fnspec(fn)
        for typ in spec.argspec.annotations.values():
            if typ in seen:
                continue
            seen.add(typ)
            dq.append(typ)

    seen = set()  # clear
    classes: t.List[t.Type[t.Any]] = []

    while dq:
        typ = dq.pop()
        if typ in seen:
            continue
        seen.add(typ)

        if isinstance(typ, type):
            classes.append(typ)

        for sub_type in _get_flatten_args(typ):
            dq.append(sub_type)

    return _get_walker(
        classes,
        config=Config(
            typeinfo_unexpected_handler=handle_unexpected_type,
            option=Config.Option(strict=False),
        ),
    )


def handle_unexpected_type(typ: t.Type[t.Any]) -> TypeInfo:
    from metashape.typeinfo import typeinfo
    from runtime import Query

    origin = t.get_origin(typ)
    if issubclass(origin, Query):
        return typeinfo(t.get_args(typ)[0])
    raise ValueError(f"unsupported type {typ}")


@lru_cache(maxsize=256)
def _get_flatten_args(typ: t.Type[t.Any]) -> t.Tuple[t.Type[t.Any]]:
    if not hasattr(typ, "__args__"):
        if hasattr(typ, "__module__") and typ.__module__ != "builtins":
            return (typ,)
        return ()  # type: ignore

    r: t.Set[t.Type[t.Any]] = set()
    for subtype in typ.__args__:
        r.update(_get_flatten_args(subtype))
    return tuple(sorted(r, key=id))  # type: ignore


def collect_types(fns: t.List[t.Callable[..., t.Any]]) -> t.List[t.Type[t.Any]]:
    return list(get_walker(fns).walk())


class Resolver:
    def __init__(self, *, refs: t.Dict[t.Type[t.Any], str]) -> None:
        self.refs = refs
        self._parsed_path_map: t.Dict[
            str, t.Dict[str, t.Tuple[str, t.Optional[t.Dict[str, t.Dict[str, t.Any]]]]]
        ] = defaultdict(dict)

    def resolve_schema(self, typ: t.Type[t.Any]) -> t.Dict[str, t.Any]:
        from metashape.outputs.openapi import detect
        from metashape.typeinfo import typeinfo

        if typ is _nonetype:
            return {}
        if hasattr(typ, "asdict"):
            return typ.asdict()

        info = typeinfo(typ, default=handle_unexpected_type)
        schema_type = detect.schema_type(info)
        # TODO: support dict, oneOf,anyOf,allOf
        if schema_type == "array":
            return {
                "type": "array",
                "items": self.refs[info.user_defined_type],
            }
        elif schema_type == "object":
            return self.refs[typ]
        else:
            return {"type": schema_type}

    def resolve_request_body(self, typ: t.Type[t.Any]) -> t.Dict[str, t.Any]:
        return {
            "requestBody": {
                "content": {"application/json": {"schema": self.resolve_schema(typ)}}
            }
        }

    def resolve_response(
        self,
        typ: t.Type[t.Any],
        *,
        description: t.Optional[str] = None,
        extra_data: t.Optional[str] = None,
    ) -> t.Dict[str, t.Any]:
        v = {
            "description": description or "Successful Response",
        }
        if typ is not _nonetype:
            v["content"] = {"application/json": {"schema": self.resolve_schema(typ)}}
        if extra_data is not None:
            v.update(extra_data)
        return v

    def resolve_path(
        self,
        path: str,
        *,
        _rx: re.Pattern = re.compile("{([^}{]+)}"),  # {xxx} or {xxx:int}
        _default_schema: t.Dict[str, t.Any] = {"type": "string"},
    ) -> t.Tuple[str, t.Optional[t.Dict[str, t.Dict[str, t.Any]]]]:
        v = self._parsed_path_map.get(path)
        if v is not None:
            return v
        if "{" not in path:
            v = self._parsed_path_map[path] = (path, None)
            return v

        schemas = {}
        for pattern in _rx.findall(path):
            name = pattern.strip()
            schema = _default_schema
            if ":" in pattern:
                name, typ = name.split(":", 1)
                name = name.strip()
                typ = typ.strip()  # todo: lookup type
                schema = {"type": typ}  # xxx
                path = path.replace(pattern, name)
            schemas[name] = schema
        return path, schemas

    def resolve_summary(self, spec: Fnspec) -> str:
        return (spec.doc or "-").split("\n\n", 1)[0]

    def resolve_description(self, spec: Fnspec) -> str:
        return (spec.doc or "-").split("\n\n", 1)[-1]


# todo: rename
class DefaultPartInjector:
    def __init__(self):
        self._error_part_injected = False

    def inject_error_part(self, root: t.Dict[str, t.Any]):
        if self._error_part_injected:
            return

        self._error_part_injected = True

        root["components"]["schemas"]["Message"] = {
            "properties": {
                "error": {"oneOf": [{"type": "string"}, {"type": "object"}]},
                "text": {"type": "string"},
            }
        }
        root["components"]["schemas"]["HTTPValidationError"] = {
            "$ref": "#/components/schemas/ValidationError",
        }
        root["components"]["schemas"]["ValidationError"] = {
            "properties": {
                "messages": {
                    "additionalProperties": {"$ref": "#/components/schemas/Message"}
                },
                "summary": {"type": "string"},
            },
            "required": ["summary"],
            "title": "ValidationError",
        }


DefaultPartInjectorT = t.TypeVar("DefaultPartInjectorT", bound=DefaultPartInjector)


def emit(
    api: runtime.API,
    *,
    title: str = "my-api",
    version: str = "0.0.0",
    license: t.Optional[str] = None,  # e.g. "mit"
    servers: t.Optional[t.List[str]] = None,
    default_error_response: t.Optional[runtime.Response] = None,
) -> t.Dict[str, t.Any]:
    from metashape.outputs.openapi.emit import scan
    from metashape.marker import mark

    root = {
        "openapi": "3.0.2",
        "info": {"title": title, "version": version},
        "paths": defaultdict(dict),
    }
    if license is not None:
        root["info"]["license"] = {"name": license}
    if servers is not None:
        root["servers"] = [{"url": url} for url in servers]

    # TODO: lazy
    routes = list(api.routes)
    stack = api._stack

    w = get_walker([fn for fn, _ in routes])
    if default_error_response is not None:
        mark(default_error_response.result)  # mark
        w.append(default_error_response.result)

    ctx = scan(w)
    root.update(ctx.result.result)  # inject #/components/schemas
    refs = ctx.state.refs

    resolver = Resolver(refs=refs)

    default_error_response_dict: t.Dict[str, t.Any] = None
    if default_error_response is not None:
        default_error_response._asdict = lambda x: resolver.resolve_schema(
            default_error_response.result
        )  # xxx
        default_error_response_dict = resolver.resolve_response(
            default_error_response, description=default_error_response.description
        )
    # default_part = DefaultPartInjector()

    for fn, metadata in routes:
        path, path_type_map = resolver.resolve_path(metadata["path"])
        method = metadata["method"]
        d = root["paths"][path][method] = {}

        spec = fnspec(fn)
        param_names = [name for name, _, _ in spec.parameters] + ["return_"]
        if path_type_map is not None:
            param_names.extend(path_type_map.keys())

        stack.push(param_names)
        c = stack.current

        args = []
        kwargs = {}
        parameter_args = []
        for name, typ, kind in spec.parameters:
            origin = runtime.Body
            first_arg = typ
            if hasattr(typ, "__origin__"):
                origin = typ.__origin__
                first_arg = typ.__args__[0]

            p = origin.__emit__(name, typ.__args__[0], d)
            if issubclass(origin, runtime.Body):
                p._asdict = partial(resolver.resolve_request_body, first_arg)
            setattr(c, name, p)

            if kind.startswith("arg"):
                args.append(p)
            else:
                kwargs[name] = p
            parameter_args.append(p)
        spec.body(*args, **kwargs)

        parameters = []
        if path_type_map is not None:
            for name, type_schema in path_type_map.items():
                p = getattr(c, name)
                p.schema = type_schema
                parameters.append(p.asdict())
        if parameter_args:
            for p in parameter_args:
                parameters.append(p.asdict())

        # basic
        d["operationId"] = spec.name
        if "summary" in metadata:
            d["summary"] = metadata["summary"]
        else:
            d["summary"] = resolver.resolve_summary(spec)
        # if "description" in metadata:
        #     d["description"] = metadata["description"]
        # else:
        #     d["description"] = resolver.resolve_description(spec)

        if "tags" in metadata:
            d["tags"] = metadata["tags"]

        # parameters
        if parameters:
            d["parameters"] = parameters

        # responses
        responses = d["responses"] = {}

        # 200
        default_status = 200
        default_asdict = resolver.resolve_response
        if spec.return_type is not None:
            return_type = spec.return_type
            # for tx.Annotated[T, DefaultStatus(N)]
            if hasattr(return_type, "__metadata__"):
                for m in return_type.__metadata__:
                    if hasattr(m, "code"):  # DefaultStatus
                        default_status = m.code
                return_type = t.get_args(return_type)[0]

        responses[str(default_status)] = default_asdict(
            return_type,
            description=c.return_.description,
            extra_data=c.return_.extra_data,
        )
        if default_error_response_dict is not None:
            responses["default"] = default_error_response_dict

        # # 422
        # default_part.inject_error_part(root)
        # responses["422"] = {
        #     "description": "Validation Error",
        #     "content": {
        #         "application/json": {
        #             "schema": {"$ref": "#/components/schemas/HTTPValidationError"}
        #         }
        #     },
        # }
        stack.pop()

    return root
