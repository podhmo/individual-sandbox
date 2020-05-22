from __future__ import annotations
import typing as t
from collections import defaultdict
from egoist.internal._fnspec import fnspec, Fnspec
from app import Metadata
from _walk import get_walker


class Resolver:
    def __init__(self, *, refs: t.Dict[t.Type[t.Any], str]) -> None:
        self.refs = refs

    def resolve_schema(self, typ: t.Type[t.Any]) -> t.Dict[str, t.Any]:
        from metashape.outputs.openapi import detect
        from metashape.typeinfo import typeinfo

        info = typeinfo(typ)
        schema_type = detect.schema_type(info)
        # TODO: support dict, oneOf,anyOf,allOf
        if schema_type == "array":
            return {
                "type": "array",
                "items": self.refs[info.user_defined_type],
            }
        else:
            return self.refs[typ]

    def resolve_request_body(self, typ: t.Type[t.Any]) -> t.Dict[str, t.Any]:
        return {
            "requestBody": {
                "content": {"application/json": {"schema": self.resolve_schema(typ)}}
            }
        }

    def resolve_doc(self, spec: Fnspec) -> str:
        return spec.doc or "-"


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
    routes: t.List[t.Tuple[t.Callable[..., t.Any], Metadata]],
    *,
    title: str = "egoist",
    version: str = "0.0.0"
) -> t.Dict[str, t.Any]:
    from metashape.outputs.openapi.emit import scan

    root = {
        "openapi": "3.0.2",
        "info": {"title": title, "version": version},
        "paths": defaultdict(dict),
    }

    # TODO: lazy
    w = get_walker([fn for fn, _ in routes])
    ctx = scan(w)
    root.update(ctx.result.result)  # inject #/components/schemas
    refs = ctx.state.refs

    resolver = Resolver(refs=refs)
    default_part = DefaultPartInjector()

    for fn, metadata in routes:
        path = metadata["path"]
        method = metadata["method"]
        d = root["paths"][path][method] = {}

        spec = fnspec(fn)

        d["summary"] = metadata.get("summary") or resolver.resolve_doc(spec)
        if "description" in metadata:
            d["description"] = metadata["description"]
        if "tags" in metadata:
            d["tags"] = metadata["tags"]

        if spec.arguments:
            typ = [typ for _, typ, _ in spec.arguments][0]
            d.update(resolver.resolve_request_body(typ))

        # responses
        responses = d["responses"] = {}

        # 200
        value = {}
        if spec.return_type is not None:
            value = resolver.resolve_schema(spec.return_type)
        responses["200"] = {
            "description": "Successful Response",
            "content": {"application/json": value},
        }

        # 422
        default_part.inject_error_part(root)
        responses["422"] = {
            "description": "Validation Error",
            "content": {
                "application/json": {
                    "schema": {"$ref": "#/components/schemas/HTTPValidationError"}
                }
            },
        }

    return root
