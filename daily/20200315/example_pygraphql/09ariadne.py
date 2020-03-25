import typing as t
from ariadne import snake_case_fallback_resolvers, make_executable_schema
from ariadne import QueryType, graphql_sync, ObjectType

from handofcats import as_command
from dictknife import loading

type_defs = """
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

Query = QueryType()


@Query.field("teams")
def resolve_teams(ob, info) -> t.List[t.Dict[str, t.Any]]:
    return ob["teams"]


Team = ObjectType("Team")


@Team.field("members")
def resolve_members(
    team, info, *, name: t.Optional[str] = None
) -> t.List[t.Dict[str, t.Any]]:
    members = team["members"]
    if name is not None:
        members = [x for x in members if x["name"] == name]
    return members


resolvers = [Query, Team]
schema = make_executable_schema(type_defs, resolvers, snake_case_fallback_resolvers)


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
    teams { __typename, name, members(name: "i") { __typename, name }}
}
    """
    ok, result = graphql_sync(schema, {"query": q}, root_value=data)
    loading.dumpfile(result)
