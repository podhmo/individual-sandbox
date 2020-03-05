from graphql import build_schema, validate, graphql_sync

schema = build_schema(
    """
    type Task {
      id: String!
      name: String
    }

    type Query {
      tasks(startswith: String): [Task]!
    }
    """
)


def resolve_tasks(obj, info, startswith=None):
    tasks = [{"name": "foo"}, {"name": "bar"}, {"name": "boo"}]
    if startswith is not None:
        tasks = [t for t in tasks if t["name"].startswith(startswith)]
    return [{"id": str(i), **x} for i, x in enumerate(tasks)]


schema.query_type.fields["tasks"].resolve = resolve_tasks

print(
    graphql_sync(
        schema,
        """
{
  tasks {
    name
  }
}
""",
    )
)
print(
    graphql_sync(
        schema,
        """
{
  tasks(startswith: "b") {
    name
  }
}
""",
    )
)
