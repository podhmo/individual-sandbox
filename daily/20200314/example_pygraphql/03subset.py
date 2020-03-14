import graphql as g


Person = g.GraphQLObjectType(
    "Person",
    lambda: {
        "name": g.GraphQLField(g.GraphQLNonNull(g.GraphQLString)),
        "age": g.GraphQLField(g.GraphQLNonNull(g.GraphQLInt)),
        "nickname": g.GraphQLField(g.GraphQLString),
        "father": g.GraphQLField(Person),
        "mother": g.GraphQLField(Person),
    },
)


class Root:
    def __init__(self, data):
        self.data = data

    def person(self, _info):
        return self.data["person"]


query_type = g.GraphQLObjectType("Query", lambda: {"person": g.GraphQLField(Person)},)


schema = g.GraphQLSchema(query_type)
q = """
{ person { name, age, nickname, father, mother } }
"""
data = {"person": {"name": "foo", "age": 20, "nickname": "F", "father": {"name": "boo", "age": 40}}}
print(g.graphql_sync(schema, q, Root(data)))
