from __future__ import annotations
import typing as t
import typing_extensions as tx
import go


class api:
    def get(self, path):
        pass


def proxy(path: str) -> t.Callable[[t.Type[t.Any]], t.Type[t.Any]]:
    def decorated(typ: t.Type[t.Any]) -> t.Type[t.Any]:
        return typ

    return decorated


class entity:  # module
    @proxy("m/entity.Todo")
    class Todo:
        Title: go.string
        Done: go.bool


class intractor:  # module
    class Todo:
        class ListInput:
            OrderBy: t.Optional[tx.Literal["title", "-title"]]

        def List(
            ir, ctx: go.Context, input: ListInput
        ) -> t.Tuple[t.List[entity.Todo], go.error]:
            api.get("/api/todo")

        def Resolve(ir, ctx: go.Context, todoID: str) -> t.Tuple[entity.Todo, go.error]:
            api.get("/api/todo/{todoID}")


# 1. for description
#  - generate interface
#  - generate api doc (?)
#
# 2. for implementation
#  - generate interactor provider
#  - generate mount handler function
#
# 3. for easiness
#  - generate skeleton of interactor (?)
#  - update interactor signature (?)
