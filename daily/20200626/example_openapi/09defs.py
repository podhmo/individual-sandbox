from __future__ import annotations
import typing as t
import typing_extensions as tx
from emit import emit
from runtime import API, Query, DefaultStatus, ErrorResponse
from dictknife import loading

# https://github.com/OAI/OpenAPI-Specification/blob/master/examples/v3.0/petstore.yaml


# definitions
int32 = t.NewType("int32", int)
int64 = t.NewType("int64", int)


class Pet:
    id: int64
    name: str
    tag: t.Optional[str]


Pets = t.List[Pet]


class Error:
    code: int32
    message: str


api = API()


# paths
@api.get("/pets", metadata={"tags": ["pets"]})
def listPets(limit: Query[int32]) -> t.List[Pet]:
    """List all pets"""
    c = api.get_current_context()

    c.limit.description = "How many items to return at one time (max 100)"
    c.limit.extra_data = {"x-xxx": "zzz"}
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
# - v2 support
if __name__ == "__main__":
    d = emit(
        api,
        title="Swagger Petstore",
        version="1.0.0",
        license="MIT",
        servers=["http://petstore.swagger.io/v1"],
        default_error_response=ErrorResponse(Error),
    )
    loading.dumpfile(d, format="yaml")
