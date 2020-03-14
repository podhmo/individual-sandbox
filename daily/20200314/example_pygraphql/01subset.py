import graphql as g


Person = g.GraphQLObjectType(
    "Person",
    lambda: {
        "name": g.GraphQLField(g.GraphQLNonNull(g.GraphQLString)),
        "age": g.GraphQLField(g.GraphQLNonNull(g.GraphQLInt)),
        "nickname": g.GraphQLField(g.GraphQLString),
    },
)


def get_person(root, _info):
    return root["person"]


query_type = g.GraphQLObjectType(
    "Query", lambda: {"person": g.GraphQLField(Person, resolve=get_person)},
)


schema = g.GraphQLSchema(query_type)
q = """
{ person { name, age, nickname } }
"""
data = {"person": {"name": "foo", "age": 20, "nickname": "F"}}
print(g.graphql_sync(schema, q, data))
