from magicalimport import import_module
from graphql import build_schema, graphql_sync

astutil = import_module("01value.py")


class Root:
    def __init__(self, d):
        self.d = d

    def tasks(self, info, *, where):
        r = []
        for task in self.d["tasks"]:
            t = astutil.ast.parse(where)
            v = astutil.StrictVisitor(env=task)
            v.visit(t)
            if v.stack[-1][0]:
                r.append(task)
        return r


schema = build_schema(
    """
type Task {
  name: String!
  completed: Boolean!
}

type Query {
  tasks(where: String): [Task]!
}
"""
)

query = """
{ tasks(where: "name.endswith('oo') and name.startswith('f')") { name } }
"""

d = {
    "tasks": [
        {"name": "foo", "completed": False},
        {"name": "bar", "completed": False},
        {"name": "boo", "completed": False},
    ]
}

result = graphql_sync(schema, query, Root(d))
print(result)
