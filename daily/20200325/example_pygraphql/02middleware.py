import graphql


type_defs = """
type Person {
    name: String!
}

type Query {
    people: [Person]!
}
"""


def resolve_people(root, info):
    return [{"name": "foo"}, {"name": "bar"}]


schema = graphql.build_schema(type_defs)
schema.get_type("Query").fields["people"].resolve = resolve_people

result = graphql.graphql_sync(schema, "query { people { name } }")
print(result)


def middleware(resolve, root_value, info: graphql.GraphQLResolveInfo, *args):
    print(info.field_name)


result = graphql.graphql_sync(
    schema, "query { people { name } }", middleware=[middleware], root_value={}
)
