from __future__ import annotations
import typing as t
import typing_extensions as tx
from metashape.outputs.openapi.types import int32, int64
from runtime import API, Query, DefaultStatus, ErrorResponse

# https://github.com/OAI/OpenAPI-Specification/blob/master/examples/v3.0/petstore.yaml

__TAGS__ = ["pets"]  # for autotags


class Pet:
    id: int64
    name: str
    tag: t.Optional[str]


class Error:
    code: int32
    message: str


api = API()


# paths
@api.get("/pets")
def listPets(limit: Query[int32]) -> t.List[Pet]:
    """List all pets"""
    pass


@api.post("/pets")
def createPets() -> tx.Annotated[None, DefaultStatus(201)]:
    """Create a pet"""
    pass


@api.get("/pets/{petId}")
def showPetById() -> Pet:
    """Info for a specific pet"""
    pass


# TODO:
# - v2 support
if __name__ == "__main__":
    from emit import emit
    from dictknife import loading

    d = emit(
        api,
        title="Swagger Petstore",
        version="1.0.0",
        license="MIT",
        servers=["http://petstore.swagger.io/v1"],
        default_error_response=ErrorResponse(Error),
        autotags=True,
    )
    loading.dumpfile(d, format="yaml")
