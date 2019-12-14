import typing as t
from magicalimport import import_module
from fastapi import APIRouter, FastAPI
from pydantic import BaseModel
from monogusa.web import runtime

cli = import_module("./cli.py", here=__file__)
router = APIRouter()


class HelloInput(BaseModel):
    name: str


@router.post("/hello", response_model=runtime.CommandOutput)
def hello(input: HelloInput) -> t.Dict[str, t.Any]:
    with runtime.handle() as s:
        cli.hello(**input.dict())  # TODO: support positional arguments?
        return s.dict()


class ByeInput(BaseModel):
    name: str


@router.post("/hello", response_model=runtime.CommandOutput)
def bye(input: ByeInput) -> t.Dict[str, t.Any]:
    with runtime.handle() as s:
        cli.bye(**input.dict())  # TODO: support positional arguments?
        return s.dict()


app = FastAPI()
app.include_router(router)


if __name__ == "__main__":
    import monogusa.web.cli as webcli

    webcli.run(app)
