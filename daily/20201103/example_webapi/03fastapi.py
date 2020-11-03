from __future__ import annotations
import typing as t
from fastapi import FastAPI
from pydantic import BaseModel


class Todo(BaseModel):
    title: str


TODOS: t.List[Todo] = []


def add_todo(todo: Todo) -> None:
    TODOS.append(todo)


def list_todo() -> t.List[Todo]:
    return TODOS


app = FastAPI()


@app.post("/todos")
async def add_todo_view(todo: Todo) -> t.List[Todo]:
    add_todo(todo)
    return list_todo()
