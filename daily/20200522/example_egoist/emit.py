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
class Handler:
    def __init__(self, root: t.Dict[str, t.Any], *, resolver: Resolver):
        self.root = root
        self.resolver = resolver
        self._filled_in_handle_error = False

    def handle_successful_response(self, *, typ: t.Type[t.Any] = None):
        value = {}
        if typ is not None:
            value = self.resolver.resolve_schema(typ)
        return {
            "200": {
                "description": "Successful Response",
                "content": {"application/json": value},
            }
        }

    def fill_in_handle_error(self):
        self.root["components"]["schemas"]["Message"] = {
            "properties": {
                "error": {"oneOf": [{"type": "string"}, {"type": "object"}]},
                "text": {"type": "string"},
            }
        }
        self.root["components"]["schemas"]["HTTPValidationError"] = {
            "$ref": "#/components/schemas/ValidationError",
        }
        self.root["components"]["schemas"]["ValidationError"] = {
            "properties": {
                "messages": {
                    "additionalProperties": {"$ref": "#/components/schemas/Message"}
                },
                "summary": {"type": "string"},
            },
            "required": ["summary"],
            "title": "ValidationError",
        }

    def handle_validation_error_response(self):
        if not self._filled_in_handle_error:
            self._filled_in_handle_error = True
            self.fill_in_handle_error()
        return {
            "422": {
                "description": "Validation Error",
                "content": {
                    "application/json": {
                        "schema": {"$ref": "#/components/schemas/HTTPValidationError"}
                    }
                },
            }
        }

    def handle_path(
        self, fn: t.Callable[..., t.Any], *, metadata: Metadata
    ) -> t.Dict[str, t.Any]:
        d = {}
        spec = fnspec(fn)

        d["summary"] = metadata.get("summary") or self.resolver.resolve_doc(spec)
        if "description" in metadata:
            d["description"] = metadata["description"]
        if "tags" in metadata:
            d["tags"] = metadata["tags"]

        if spec.arguments:
            typ = [typ for _, typ, _ in spec.arguments][0]
            d.update(self.resolver.resolve_request_body(typ))

        responses = d["responses"] = {}
        responses.update(self.handle_successful_response(typ=spec.return_type))
        responses.update(self.handle_validation_error_response())

        return d


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
        "components": {"schemas": {}},
    }
    paths = root["paths"]

    # TODO: lazy
    fns = [fn for fn, _ in routes]
    w = get_walker(fns)

    ctx = scan(w)
    root.update(ctx.result.result)

    h = Handler(root, resolver=Resolver(refs=ctx.state.refs))
    for fn, metadata in routes:
        path = metadata["path"]
        method = metadata["method"]
        paths[path][method] = h.handle_path(fn, metadata=metadata)
    return root
