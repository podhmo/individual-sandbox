from molten import App, Route, field, schema
from typing import Optional


@schema
class Todo:
    id: Optional[int] = field(response_only=True)
    description: str
    status: str = field(choices=["todo", "done"], default="todo")


def create_todo(todo: Todo) -> Todo:
    # Do something to store the todo here...
    return todo


app = App(routes=[Route("/todos", create_todo, method="POST")])
