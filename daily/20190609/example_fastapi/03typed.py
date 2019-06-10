import typing as t
from fastapi import FastAPI, APIRouter
from pydantic import BaseModel


class User(BaseModel):
    id: t.Optional[str] = None
    name: str
    createdAt: t.Optional[str] = None  # datetime


route = APIRouter()


@route.get("/")
async def root() -> t.Dict[str, str]:
    return {"message": "Hello World"}


@route.post("/users")
async def create_user(user: User) -> User:
    return user


app = FastAPI()
app.include_router(route)
if __name__ == "__main__":
    import uvicorn  # type: ignore

    uvicorn.run(app, port=8080, debug=True)
