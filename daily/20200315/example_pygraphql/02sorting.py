import graphql as g
from handofcats import as_command
from dictknife import loading

person_order_input_enum = g.GraphQLEnumType(
    "PersonOrderInput",
    {
        "name_ASC": g.GraphQLEnumValue("name_ASC"),
        "name_DESC": g.GraphQLEnumValue("name_DESC"),
    },
)

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
        "people": g.GraphQLField(
            g.GraphQLList(Person),
            args={"order_by": g.GraphQLArgument(person_order_input_enum)},
        )
    },
)


class Root:
    def __init__(self, data):
        self.data = data

    def people(self, info, *, order_by):
        if order_by == "name_DESC":
            return sorted(self.data["people"], key=lambda d: d["name"], reverse=True)
        elif order_by == "name_ASC":
            return sorted(self.data["people"], key=lambda d: d["name"])


schema = g.GraphQLSchema(query_type)


@as_command
def run():
    q = """\
{
  desc_people: people(order_by: name_DESC) { name, age, nickname },
  asc_people: people(order_by: name_ASC) { name, age, nickname }
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
    loading.dumpfile(result.data)
