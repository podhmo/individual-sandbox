import graphql as g
from handofcats import as_command

Person = g.GraphQLObjectType(
    "Person",
    lambda: {
        "name": g.GraphQLField(g.GraphQLNonNull(g.GraphQLString)),
        "age": g.GraphQLField(g.GraphQLNonNull(g.GraphQLInt)),
        "nickname": g.GraphQLField(g.GraphQLString),
    },
)


query_type = g.GraphQLObjectType(
    "Query", lambda: {"people": g.GraphQLField(g.GraphQLList(Person))},
)


class Root:
    def __init__(self, data):
        self.data = data

    def people(self, info):
        return self.data["people"]


schema = g.GraphQLSchema(query_type)
# # graphql.langunage.ast.DocumenNode, validateが受け取るのはこれ
# g.validate(schema, g.parse(q))


@as_command
def run():
    q = """\
    { people { name, age, nickname } }
    """

    data = {
        "people": [
            {"name": "foo", "age": 20, "nickname": "F"},
            {"name": "bar", "age": 20},
        ]
    }

    # 実行はこう
    print(g.graphql_sync(schema, q, Root(data)))
