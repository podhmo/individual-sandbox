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
        actions = []
        unfinished = set()
        d = {}
        futures = {}

        fut = futures["teams"] = asyncio.ensure_future(resolve_teams())
        unfinished.add(fut)
        actions.append(fut)

        def _callback(fut):
            d["teams"] = fut.result()
            unfinished.remove(fut)

        fut.add_done_callback(_callback)

        async def run_resolve_members():
            teams = await futures["teams"]
            afns = [resolve_members(team) for team in teams]
            return zip(teams, await asyncio.gather(*afns))

        fut = futures["members"] = asyncio.ensure_future(run_resolve_members())
        unfinished.add(fut)
        actions.append(fut)

        def _callback(fut):
            # xxx:
            for team, members in fut.result():
                team["members"] = members
            unfinished.remove(fut)

        fut.add_done_callback(_callback)

        async def waiter():
            while len(unfinished) > 0:
                await asyncio.sleep(0.01)

        actions.append(waiter())
        await asyncio.gather(*actions)
        return d

    print(asyncio.run(do_task(), debug=True))
