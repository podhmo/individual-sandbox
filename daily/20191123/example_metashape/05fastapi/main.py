import typing as t
from fastapi import FastAPI
from pydantic import BaseModel


app = FastAPI(debug=True)


class Message(BaseModel):
    Hello: str


@app.get("/", response_model=Message)
def read_root():
    return {"Hello": "World"}


class Item(BaseModel):
    item_id: int
    q: t.Optional[str]


@app.get("/items/{item_id}", response_model=Item)
def read_item(item_id: int, q: str = None):
    return {"item_id": item_id, "q": q}
