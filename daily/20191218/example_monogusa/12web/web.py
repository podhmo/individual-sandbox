import typing as t
from pydantic import BaseModel
from fastapi import (
    APIRouter,
    Depends,
    FastAPI
)
from monogusa.web import (
    runtime
)
import 12web.commands


router = APIRouter()


class Hello(BaseModel):
    """
    auto generated class from 12web.commands.hello
    """
    name: str


@router.post("/hello", response_model=runtime.CommandOutput)
def hello(input: Hello) -> t.Dict[str, t.Any]:
    """
    hello world
    """
    with runtime.handle() as s:
        12web.commands.hello(**input.dict())
        return s.dict()


@router.post("/byebye", response_model=runtime.CommandOutput)
def byebye() -> t.Dict[str, t.Any]:
    with runtime.handle() as s:
        12web.commands.byebye()
        return s.dict()


def main(app: t.Optional[FastAPI]=None):
    from monogusa.web import cli
    if app is None:
        app = FastAPI()
        app.include_router(router)
    cli.run(app, where=None)


app = FastAPI()
app.include_router(router)
if __name__ == '__main__':
    main(app=app)
