from __future__ import annotations
import typing as t
from collections import defaultdict
from egoist.internal._fnspec import fnspec, Fnspec
from app import Metadata

if t.TYPE_CHECKING:
    from metashape.analyze.walker import Walker
    from metashape.outputs.openapi.emit import Context


class Resolver:
    def __init__(self, ctx: Context) -> None:
        self.ctx = ctx

    def resolve_schema(self, typ: t.Type[t.Any]) -> t.Dict[str, t.Any]:
        return self.ctx.state.refs[typ]

    def resolve_request_body(self, spec: Fnspec) -> t.Dict[str, t.Any]:
        typ = [typ for _, typ, _ in spec.arguments][0]
        return {
            "requestBody": {
                "content": {"application/json": {"schema": self.resolve_schema(typ)}}
            }
        }

    def resolve_doc(self, spec: Fnspec) -> str:
        return spec.doc or "-"


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


def get_walker(classes: t.List[t.Type[t.Any]]) -> Walker:
    from metashape.runtime import get_walker
    from metashape.analyze.config import Config

    return get_walker(classes, config=Config(option=Config.Option(strict=False)))


def emit(
    w: Walker,
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
    ctx = scan(w)
    r = Resolver(ctx)
    root.update(ctx.result.result)
    h = Handler(root, resolver=r)

    for fn, metadata in routes:
        spec = fnspec(fn)
        path = metadata["path"]
        method = metadata["method"]

        d = paths[path][method] = {}
        d["summary"] = metadata.get("summary") or r.resolve_doc(spec)
        if "description" in metadata:
            d["description"] = metadata["description"]
        if "tags" in metadata:
            d["tags"] = metadata["tags"]
        d.update(r.resolve_request_body(spec))

        responses = d["responses"] = {}
        responses.update(h.handle_successful_response(typ=spec.return_type))
        responses.update(h.handle_validation_error_response())
    return root
