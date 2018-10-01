from molten import App, Include, Route


class TodoManager:
    def __init__(self, db: DB) -> None:
        self.db = db

    def get_all(self) -> List[Todo]:
        ...

    def save(self, todo: Todo) -> Todo:
        ...


class TodoManagerComponent:
    is_cacheable = True
    is_singleton = True

    def can_handle_parameter(self, parameter: Parameter) -> bool:
        return parameter.annotation is TodoManager

    def resolve(self, db: DB) -> TodoManager:
        return TodoManager(db)


def list_todos(todo_manager: TodoManager) -> List[Todo]:
    return todo_manager.get_all()


def create_todo(todo: Todo, todo_manager: TodoManager) -> Todo:
    return todo_manager.save(todo)


app = App(
    components=[
        DBComponent(),
        TodoManagerComponent(),
    ],
    routes=[
        Include("/todos", [
            Route("/", list_todos),
            Route("/", create_todo, method="POST"),
        ]),
    ],
)
