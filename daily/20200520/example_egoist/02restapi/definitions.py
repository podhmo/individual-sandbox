import typing as t
from app import App
from handofcats import as_subcommand
from collections import defaultdict
from egoist.internal._fnspec import fnspec, Fnspec

app = App()


class Article:
    title: str
    content: str


@app.post("/api/articles", metadata={"tags": ["xxx"]})
def create_article(article: Article) -> Article:
    pass


@as_subcommand
def describe():
    for fn, metadata in app.handlers:
        print(fn, metadata)


class Resolver:
    def resolve_schema(self, typ: t.Type[t.Any]) -> t.Dict[str, t.Any]:
        return {}

    def resolve_request_body(self, spec: Fnspec) -> t.Dict[str, t.Any]:
        typ = None
        return {
            "requestBody": {
                "content": {"application/json": {"content": {"schema": typ}}}
            }
        }

    def resolve_doc(self, spec: Fnspec) -> str:
        return spec.doc or "-"


class Handler:
    def __init__(self, root: t.Dict[str, t.Any]):
        self.root = root
        self._filled_in_handle_error = False

    def handle_successful_response(
        self, *, value: t.Optional[t.Dict[str, t.Any]] = None
    ):
        return {
            "200": {
                "description": "Successful Response",
                "content": {"application/json": value or {}},
            }
        }

    def fill_in_handle_error(self):
        self.root["components"]["schemas"]["Messages"] = {
            "properties": {
                "error": {"oneOf": [{"type": "string"}, {"type": "object"}]},
                "text": {"type": "string"},
            },
            "required": ["summary", "messages"],
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


@as_subcommand
def emit(*, title: str = "egoist", version: str = "0.0.0"):
    from dictknife import loading

    r = Resolver()
    root = {
        "openapi": "3.0.2",
        "info": {"title": title, "version": version},
        "paths": defaultdict(dict),
        "components": {"schemas": {}},
    }
    paths = root["paths"]
    h = Handler(root)

    for fn, metadata in app.routes:
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
        responses.update(h.handle_successful_response())
        responses.update(h.handle_validation_error_response())
    loading.dumpfile(root)


as_subcommand.run()
