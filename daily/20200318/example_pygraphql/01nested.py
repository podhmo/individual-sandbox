import graphql as g
from handofcats import as_command
from dictknife import loading
from minidb import Table

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


def resolve_query__teams(root, info):
    tables = info.root_value
    return [d for d in tables["teams"].find_all()]


def resolve_team__members(team, info, *, name=None):
    tables = info.root_value
    where = [lambda d: d["team_id"] == team["name"]]
    if name is not None:
        where.append(lambda d: d["name"] == name)
    return tables["members"].find_all(where=lambda d: all(p(d) for p in where))


def resolve_user__name(user, info):
    print("@", user)
    return user["name"]


@as_command
def run():
    schema.get_type("Query").fields["teams"].resolve = resolve_query__teams
    schema.get_type("Team").fields["members"].resolve = resolve_team__members
    schema.get_type("User").fields["name"].resolve = resolve_user__name

    class Teams(Table):
        pk = "name"

    class Members(Table):
        pk = "name"

    tables = {
        "teams": Teams([{"name": "x"}, {"name": "y"}]),
        "members": Members(
            [
                {"age": 10, "name": "a", "team_id": "x"},
                {"age": 10, "name": "b", "team_id": "x"},
                {"age": 10, "name": "c", "team_id": "x"},
                {"age": 10, "name": "i", "team_id": "y"},
                {"age": 10, "name": "j", "team_id": "y"},
                {"age": 10, "name": "k", "team_id": "y"},
            ]
        ),
    }

    q = """\
{
    teams { name, members { name }}
}
    """

    result = g.graphql_sync(schema, q, tables)
    print(result.errors)
    loading.dumpfile(result.data)
