import graphql as g
from handofcats import as_command
from minidb import Table

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
    def __init__(self, tables):
        self.tables = tables

    def people(self, info):
        return self.tables["people"].find_all()


schema = g.GraphQLSchema(query_type)


@as_command
def run():
    q = """\
    { people { name, age, nickname } }
    """

    class People(Table):
        pk = "name"

    people = People(
        [{"name": "foo", "age": 20, "nickname": "F"}, {"name": "bar", "age": 20},]
    )

    data = {"people": people}

    print(g.graphql_sync(schema, q, Root(data)))
