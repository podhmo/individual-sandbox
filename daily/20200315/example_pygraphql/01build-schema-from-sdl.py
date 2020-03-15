import graphql as g
from handofcats import as_command

schema = g.build_schema(
    """
type Person {
  name: String!
  age: Int!
  nickname: String
}

type Query {
  people: [Person!]!
}
"""
)


class Root:
    def __init__(self, data):
        self.data = data

    def people(self, info):
        return self.data["people"]


@as_command
def run():
    data = {
        "people": [
            {"name": "foo", "age": 20, "nickname": "F"},
            {"name": "bar", "age": 20},
        ]
    }

    q = """\
    { people { name, age, nickname } }
    """
    # 実行はこう
    # g.validate(schema, g.parse(q))
    print(g.graphql_sync(schema, q, Root(data)))
