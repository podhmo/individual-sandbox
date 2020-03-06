from graphql import build_schema, graphql_sync


class Root:
    def __init__(self, d):
        self.d = d

    def tasks(self, info, *, where):
        r = []
        for t in self.d["tasks"]:
            # unsafe
            if eval(f"{where}", {"__builtins__": None}, {"name": t["name"]}):
                r.append(t)
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
{ tasks(where: "name.endswith('oo')") { name } }
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
