# this module is generated by monogusa.web.codegen
import typing as t
from pydantic import BaseModel
from fastapi import APIRouter, Depends, FastAPI
from monogusa.web import runtime
import commands


router = APIRouter()


def database_url() -> str:
    return commands.database_url()


def db(database_url: str = Depends(database_url)) -> str:
    return commands.db(database_url)


class HelloInput(BaseModel):
    name: str = "world"


@router.post("/hello", response_model=runtime.CommandOutput)
def hello(input: HelloInput, db: commands.DB = Depends(db)) -> t.Dict[str, t.Any]:
    with runtime.handle() as s:
        # TODO: support positional arguments? (DI)
        commands.hello(db, **input.dict())
        return s.dict()


class ByebyeInput(BaseModel):
    name: str


@router.post("/byebye", response_model=runtime.CommandOutput)
def byebye(input: ByebyeInput, db: commands.DB = Depends(db)) -> t.Dict[str, t.Any]:
    with runtime.handle() as s:
        # TODO: support positional arguments? (DI)
        commands.byebye(db, **input.dict())
        return s.dict()


def main(app: FastAPI):
    from monogusa.web import cli

    cli.run(app)


app = FastAPI()
app.include_router(router)
if __name__ == "__main__":
    main(app=app)
