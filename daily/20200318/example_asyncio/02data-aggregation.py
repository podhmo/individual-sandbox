import logging
import asyncio
from handofcats import as_command

logger = logging.getLogger(__name__)


async def resolve_teams():
    await asyncio.sleep(0.5)
    return [{"name": "foo"}, {"name": "bar"}]


async def resolve_members(team):
    if team["name"] == "foo":
        await asyncio.sleep(0.3)
        return [{"name": "x"}, {"name": "y"}, {"name": "z"}]
    elif team["name"] == "bar":
        await asyncio.sleep(0.5)
        return [{"name": "i"}, {"name": "j"}, {"name": "k"}]
    raise NotImplementedError("hmm")


@as_command
def run():
    async def do_task():
        futures = {
            "root": asyncio.Future(),
            "teams": asyncio.Future(),
            "members": asyncio.Future(),
        }
        d = {}
        futures["root"].set_result(None)

        async def run_resolve_teams():
            fut = futures["teams"]
            try:
                await futures["root"]
                d["teams"] = result = await resolve_teams()
                fut.set_result(result)
            except Exception as e:
                fut.set_exception(e)

        async def run_resolve_members():
            fut = futures["members"]
            try:
                teams = await futures["teams"]
                afns = [resolve_members(team) for team in teams]
                result = await asyncio.gather(*afns)
                for team, members in zip(teams, result):
                    team["members"] = members
                fut.set_result(result)
            except Exception as e:
                fut.set_exception(e)

        asyncio.ensure_future(run_resolve_teams())
        asyncio.ensure_future(run_resolve_members())
        await asyncio.gather(*futures.values())
        return d

    print(asyncio.run(do_task(), debug=True))
