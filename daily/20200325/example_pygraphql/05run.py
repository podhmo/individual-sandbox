import graphql


schema = graphql.build_schema(
    """
type Person {
    name: String!
}

type Query {
    people: [Person]!
}
"""
)


data = {"people": [{"name": "foo"}, {"name": "bar"}]}
# default_field_resolverがdictを見てくれるので以下と同じ
# schema.get_type("Query").fields["people"].resolve = lambda root, info: data["people"]

result = graphql.graphql_sync(schema, "{ people { name }}", data)
print(result)
# ExecutionResult(data={'people': [{'name': 'foo'}, {'name': 'bar'}]}, errors=None)
