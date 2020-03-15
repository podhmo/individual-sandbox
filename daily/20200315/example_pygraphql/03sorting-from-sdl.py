import graphql as g
from handofcats import as_command
from dictknife import loading

schema = g.build_schema(
    """
enum PersonOrderInput {
  name_ASC
  name_DESC
}

type Person {
  name: String!
  age: Int!
  nickname: String
}

type Query {
  people(
    order_by: PersonOrderInput
  ): [Person!]!
}
"""
)
for enum_type_name in ["PersonOrderInput"]:
    for name, value in schema.get_type(enum_type_name).values.items():
        value.value = name


class Root:
    def __init__(self, data):
        self.data = data

    def people(self, info, *, order_by):
        if order_by == "name_DESC":
            return sorted(self.data["people"], key=lambda d: d["name"], reverse=True)
        elif order_by == "name_ASC":
            return sorted(self.data["people"], key=lambda d: d["name"])


@as_command
def run():
    data = {
        "people": [
            {"name": "boo", "age": 20},
            {"name": "foo", "age": 20, "nickname": "F"},
            {"name": "bar", "age": 20},
        ]
    }

    q = """\
{
  desc_people: people(order_by: name_DESC) { name, age, nickname },
  asc_people: people(order_by: name_ASC) { name, age, nickname }
}
    """

    result = g.graphql_sync(schema, q, Root(data))
    loading.dumpfile(result.data)
