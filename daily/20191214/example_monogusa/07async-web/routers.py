import typing as t
from magicalimport import import_module
from fastapi import APIRouter
from monogusa.web import runtime

cli = import_module("./cli.py", here=__file__)
router = APIRouter()


@router.post("/hello", response_model=runtime.CommandOutput)
async def hello() -> t.Dict[str, t.Any]:
    with runtime.handle() as s:
        await cli.hello()  # TODO: support positional arguments?
        return s.dict()
