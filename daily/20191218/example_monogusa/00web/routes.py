import typing as t
from pydantic import BaseModel
from fastapi import APIRouter, Depends
from monogusa.web import runtime
import commands


router = APIRouter()


class HelloInput(BaseModel):
    name: str = "world"


@router.post("hello", response_model=runtime.CommandOutput)
def hello(
    input: HelloInput,
    writer: commands.Writer = Depends(commands.writer),  # TODO: more deps?
) -> t.Dict[str, t.Any]:
    with runtime.handle() as s:
        commands.hello(writer, **input.dict())
        return s.dict()
