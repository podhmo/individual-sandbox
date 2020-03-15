import typing as t
import graphql as g
from handofcats import as_command
from dictknife import loading

schema = g.build_schema(
    """
type Person {
  name: String!
  age: Int!
  nickname: String
}

type Query {
  person(
    name: String
  ): Person
}
"""
)

Person = t.Dict[str, t.Any]


class Root:
    def __init__(self, data):
        self.data = data

    def person(self, info, *, name: t.Optional[str] = None) -> t.Optional[Person]:
        if name is None:
            return None
        for row in self.data["people"]:
            if name == row["name"]:
                return row
        return None


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
  foo: person(name: "foo") { name, age, nickname },
  xxx: person(name: "xxx") { name, age, nickname },
}
    """

    result = g.graphql_sync(schema, q, Root(data))
    print(result.errors)
    loading.dumpfile(result.data)
