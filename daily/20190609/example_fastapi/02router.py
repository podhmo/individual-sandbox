from fastapi import FastAPI, APIRouter
from pydantic import BaseModel


class User(BaseModel):
    id: str = None
    name: str
    createdAt: str = None  # datetime


route = APIRouter()


@route.get("/")
async def root():
    return {"message": "Hello World"}


@route.post("/users")
async def create_user(user: User):
    return user


app = FastAPI()
app.include_router(route)
if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, port=8080, debug=True)
