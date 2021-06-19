import typing as t
from collections import defaultdict
import asyncio
import logging
import contextlib

logger = logging.getLogger(__name__)

data = {
    "teams": {
        0: {"id": 0, "name": "A", "members": [0, 1, 2, 3]},
        1: {"id": 1, "name": "B", "members": [0, 4, 5]},
    },
    "users": {
        0: {"id": 0, "name": "x"},
        1: {"id": 1, "name": "y"},
        2: {"id": 2, "name": "z"},
        3: {"id": 3, "name": "i"},
        4: {"id": 4, "name": "j"},
        5: {"id": 5, "name": "k"},
    },
}


async def create_data_loader(
    q: asyncio.Queue[t.Tuple[int, asyncio.Future]],
    bulk_get,
    *,
    buffering_time: float = 0.3
):
    try:
        logger.info("start data lodader %s", bulk_get)
        i = 0

        waiter_dict = defaultdict(list)
        while True:
            logger.debug("tick %d, data lodader %s", i, bulk_get)
            waiter_dict.clear()

            i += 1
            id, fut = await q.get()
            waiter_dict[id].append(fut)

            try:
                while True:
                    id, fut = await asyncio.wait_for(q.get(), timeout=buffering_time)
                    waiter_dict[id].append(fut)
            except asyncio.TimeoutError:
                pass

            results = await bulk_get(list(waiter_dict.keys()))

            seen = []
            for id, d in results.items():
                seen.append(id)
                futs = waiter_dict.get(id)
                if futs is None:
                    logger.debug("unexpected id %d, in data loader %s", id, bulk_get)
                    continue

                for fut in futs:
                    fut.set_result(d)

            for unseen_id in set(waiter_dict.keys()).difference(seen):
                logger.debug("cancel id %d, in data loader %s", id, bulk_get)
                for fut in waiter_dict[unseen_id]:
                    fut.cancel()

    except Exception as e:
        logger.exception(e)
    finally:
        logger.info("stop data lodader %s", bulk_get)


User = t.Dict[t.Any, t.Any]
Team = t.Dict[t.Any, t.Any]


async def get_user(id: int) -> User:
    logger.info("get user %d", id)
    await asyncio.sleep(0.5)
    return data["users"][id]


async def bulk_get_user(ids: t.Sequence[int]) -> t.Dict[int, User]:
    logger.info("bulk get user %s", ids)
    await asyncio.sleep(0.5)
    source = data["users"]
    r = {id: source[id] for id in ids if id in source}
    logger.debug("bulk get user %s ... ok", ids)
    return r


async def get_team(id: int) -> Team:
    logger.info("get team %d", id)
    await asyncio.sleep(0.5)
    return data["teams"][id]


async def bulk_get_team(ids: t.Sequence[int]) -> t.Dict[int, Team]:
    logger.info("bulk get team %s", ids)
    await asyncio.sleep(0.5)
    source = data["teams"]
    r = {id: source[id] for id in ids if id in source}
    logger.debug("bulk get team %s ... ok", ids)
    return r


async def list_teams() -> t.List[Team]:
    logger.info("list team")
    await asyncio.sleep(0.5)
    return list(data["teams"].keys())


async def run():
    logger.info("start ...")
    d = {}

    loop = asyncio.get_event_loop()

    with contextlib.ExitStack() as s:
        q0 = asyncio.Queue()
        t0 = loop.create_task(create_data_loader(q0, bulk_get_team))
        s.callback(t0.cancel)
        q1 = asyncio.Queue()
        t1 = loop.create_task(create_data_loader(q1, bulk_get_user))
        s.callback(t1.cancel)

        async def _get_team(team_id):
            fut = asyncio.Future()
            await q0.put((team_id, fut))
            r = await fut
            logger.debug("wait team %s -- %s", team_id, r)
            return r

        async def _get_user(user_id):
            fut = asyncio.Future()
            await q1.put((user_id, fut))
            r = await fut
            logger.debug("wait user %s -- %s", user_id, r)
            return r

        async def _get_full_data(team_id):
            team = await _get_team(team_id)
            sd = {"name": team["name"], "members": []}

            futs = []
            for member_id in team["members"]:
                futs.append(_get_user(member_id))
            r = await asyncio.gather(*futs)
            for member in r:
                sd["members"].append({"name": member["name"]})
            return sd

        team_ids = await list_teams()
        d = await asyncio.gather(*[_get_full_data(team_id) for team_id in team_ids])
        logger.info("... end")
        for i, sd in enumerate(d):
            print({"i": i, "data": sd})


def main():
    logging.basicConfig(
        # level=logging.DEBUG, format="%(relativeCreated)d	" + logging.BASIC_FORMAT
        level=logging.INFO,
        format="%(relativeCreated)d	" + logging.BASIC_FORMAT,
    )
    asyncio.run(run(), debug=True)


main()
