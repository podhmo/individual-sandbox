from fastapi import FastAPI
from pydantic import BaseModel


class User(BaseModel):
    id: str = None
    name: str
    createdAt: str = None  # datetime


app = FastAPI()


@app.get("/")
async def root():
    return {"message": "Hello World"}


@app.post("/users")
async def create_user(user: User):
    return user


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, port=8080, debug=True)
