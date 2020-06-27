from __future__ import annotations
import typing as t
from emit import emit
from runtime import API, Query
from dictknife import loading

# https://github.com/OAI/OpenAPI-Specification/blob/master/examples/v3.0/petstore.yaml


# definitions
class Pet:
    id: str
    name: str
    tag: t.Optional[str]


Pets = t.List[Pet]


class Error:
    code: int  # int32
    message: str


api = API()


# paths
@api.get("/pets", metadata={"tags": ["pets"]})
def listPets(limit: Query[int]) -> t.List[Pet]:
    """list all pets"""
    c = api.get_current_context()

    c.limit.description = "How many items to return at one time (max 100)"
    c.limit.required = False
    c.limit.schema = {"type": "integer", "format": "int32"}


# TODO:
# - servers
# - responses description
# - responses headers
# - default error handling
# - path types
d = emit(api, title="Swagger Petstore", version="1.0.0")
loading.dumpfile(d, format="yaml")
