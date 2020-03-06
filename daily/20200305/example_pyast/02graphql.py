from graphql import build_schema, graphql_sync


class Root:
    def __init__(self, d):
        self.d = d

    def tasks(self, info):
        return self.d["tasks"]


schema = build_schema(
    """
type Task {
  name: String!
  completed: Boolean!
}

type Query {
  tasks: [Task]!
}
"""
)

query = """
{ tasks { name } }
"""

d = {
    "tasks": [
        {"name": "foo", "completed": False},
        {"name": "bar", "completed": False},
        {"name": "boo", "completed": False},
    ]
}

# expected: <graphql.type.schema.GraphQLSchema object at 0x10ebcd730>
# actual: DocumentNode at 0:83
result = graphql_sync(schema, query, Root(d))
print(result)
