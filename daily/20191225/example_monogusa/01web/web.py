# this module is generated by monogusa.web.codegen
import commands
from fastapi import (
    APIRouter,
    Depends,
    FastAPI
)
import typing as t
from pydantic import BaseModel
from monogusa.web import runtime


router = APIRouter()


@router.post("/hello", response_model=runtime.CommandOutput)
def hello() -> t.Dict[str, t.Any]:
    with runtime.handle() as s:
        commands.hello()
    return s.dict()


def main(app: FastAPI):
    from monogusa.web import cli
    cli.run(app)


app = FastAPI()
app.include_router(router)


if __name__ == '__main__':
    main(app=app)