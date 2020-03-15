import typing as t
import graphql as g
from handofcats import as_command
from dictknife import loading

Person = g.GraphQLObjectType(
    "Person",
    lambda: {
        "name": g.GraphQLField(g.GraphQLNonNull(g.GraphQLString)),
        "age": g.GraphQLField(g.GraphQLNonNull(g.GraphQLInt)),
        "nickname": g.GraphQLField(g.GraphQLString),
    },
)


query_type = g.GraphQLObjectType(
    "Query",
    lambda: {
        "person": g.GraphQLField(
            Person, args={"name": g.GraphQLArgument(g.GraphQLString)},
        )
    },
)

Person = t.Dict[str, t.Any]


class Root:
    def __init__(self, data):
        self.data = data

    def person(self, info, *, name: t.Optional[str] = None) -> Person:
        if name is None:
            return None
        for row in self.data["people"]:
            if name == row["name"]:
                return row
        return None


schema = g.GraphQLSchema(query_type)


@as_command
def run():
    q = """\
{
 foo: person(name: "foo") { name, age, nickname }
 xxx: person(name: "xxx") { name, age, nickname }
}
    """

    data = {
        "people": [
            {"name": "boo", "age": 20},
            {"name": "foo", "age": 20, "nickname": "F"},
            {"name": "bar", "age": 20},
        ]
    }

    result = g.graphql_sync(schema, q, Root(data))
    print(result.errors)
    loading.dumpfile(result.data)
