import graphql as g
from handofcats import as_command
from dictknife import loading

schema = g.build_schema(
    """
type Team {
  name: String!
  members(name: String): [User]
}

type User {
  name: String!
  age: Int!
}

type Query {
  teams: [Team!]!
}
"""
)


class Root:
    def __init__(self, data):
        self.data = data

    def teams(self, info):
        print("@@", "team")
        return [Team(x) for x in self.data["teams"]]


class Team:
    def __init__(self, data):
        self.data = data

    def name(self, info):
        print("@", "name")
        return self.data["name"]

    def members(self, info, *, name=None):
        print("@", "members")
        members = self.data["members"]
        if name is not None:
            members = [x for x in members if x["name"] == name]
        return members


@as_command
def run():
    data = {
        "teams": [
            {
                "name": "x",
                "members": [
                    {"age": 10, "name": "a"},
                    {"age": 10, "name": "b"},
                    {"age": 10, "name": "z"},
                ],
            },
            {
                "name": "y",
                "members": [
                    {"age": 10, "name": "i"},
                    {"age": 10, "name": "j"},
                    {"age": 10, "name": "k"},
                ],
            },
        ]
    }

    q = """\
{
    teams { name, members(name: "i") { name }}
}
    """

    result = g.graphql_sync(schema, q, Root(data))
    print(result.errors)
    loading.dumpfile(result.data)
