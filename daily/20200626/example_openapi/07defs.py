from __future__ import annotations
import typing as t
import typing_extensions as tx
from emit import emit
from runtime import API, Query, DefaultStatus, ErrorResponse
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
    """List all pets"""
    c = api.get_current_context()

    c.limit.description = "How many items to return at one time (max 100)"
    c.limit.required = False
    c.limit.schema = {"type": "integer", "format": "int32"}

    c.return_.description = "A paged array of pets"
    c.return_.extra_data = {
        "headers": {
            "x-next": {
                "description": "A link to the next page of responses",
                "schema": {"type": "string"},
            }
        }
    }


@api.post("/pets", metadata={"tags": ["pets"]})
def createPets() -> tx.Annotated[None, DefaultStatus(201)]:
    """Create a pet"""
    c = api.get_current_context()
    c.return_.description = "Null response"


@api.get("/pets/{petId}", metadata={"tags": ["pets"]})
def showPetById() -> Pet:
    """Info for a specific pet"""
    c = api.get_current_context()

    c.petId.description = "The id of the pet to retrieve"

    c.return_.description = "Expected response to a valid request"


# TODO:
# - servers
# - setting type as NewType
# - v2 support
d = emit(
    api,
    title="Swagger Petstore",
    version="1.0.0",
    license="MIT",
    servers=["http://petstore.swagger.io/v1"],
    default_error_response=ErrorResponse(Error),
)
loading.dumpfile(d, format="yaml")
