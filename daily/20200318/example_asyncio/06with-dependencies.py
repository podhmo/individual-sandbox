import logging
import asyncio
import minidb
import contextlib
from handofcats import as_command

logger = logging.getLogger(__name__)


class Users(minidb.AsyncTable):
    pk = "name"


class Teams(minidb.AsyncTable):
    pk = "name"


class TeamToMembers(minidb.AsyncTable):
    pk = "team"


users = Users(
    [
        {"name": "x"},
        {"name": "y"},
        {"name": "z"},
        {"name": "i"},
        {"name": "j"},
        {"name": "k"},
    ]
)
teams = Teams([{"name": "foo"}, {"name": "bar"}])
team_to_members = TeamToMembers(
    [
        {"team": "foo", "members": ["x", "y", "z"]},
        {"team": "bar", "members": ["i", "j", "k"]},
    ]
)


async def resolve_teams():
    return await teams.find_all()


async def resolve_members(team):
    xref = await team_to_members.find_one(in_=[team["name"]])
    return await resolve_members.find_all(in_=[xref["members"]])


class Graph:
    def __init__(self):
        self.futures = {
            "root": asyncio.Future(),
            "teams": asyncio.Future(),
            "members": asyncio.Future(),
        }
        self.d = {}

    def deps(self, *, input, output):
        def _deps(afn):
            async def do():
                fut = self.futures[input]
                try:
                    result = self.d[output] = await afn(await fut)
                    fut.set_result(result)
                except Exception as e:
                    fut.set_exception(e)
            return do
        return _deps


async def do_task():
    g = Graph()
    g.futures["root"].set_result(None)

    @g.deps(input="root", output="teams")
    async def run_resolve_teams(root):
        return await resolve_teams()

    @g.deps(input="teams", output="members")
    async def run_resolve_members(teams):
        afns = [resolve_members(team) for team in teams]
        result = await asyncio.gather(*afns)
        for team, members in zip(teams, result):
            team["members"] = members
        return result

    asyncio.ensure_future(run_resolve_teams())
    asyncio.ensure_future(run_resolve_members())
    await asyncio.gather(*g.futures.values())


print(asyncio.run(do_task(), debug=True))
