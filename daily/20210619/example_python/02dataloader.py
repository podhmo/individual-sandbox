import typing as t
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

        while True:
            logger.info("tick %d, data lodader %s", i, bulk_get)
            i += 1
            futs = {}
            id, fut = await q.get()
            futs[id] = fut

            try:
                while True:
                    id, fut = await asyncio.wait_for(q.get(), timeout=buffering_time)
                    futs[id] = fut
            except asyncio.TimeoutError:
                pass

            results = await bulk_get(list(futs.keys()))
            for id, d in results.items():
                fut = futs.get(id)
                if fut is None:
                    fut.cancel()
                    continue
                fut.set_result(d)
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
    return {id: source[id] for id in ids if id in source}


async def get_team(id: int) -> Team:
    logger.info("get team %d", id)
    await asyncio.sleep(0.5)
    return data["teams"][id]


async def bulk_get_team(ids: t.Sequence[int]) -> t.Dict[int, Team]:
    logger.info("bulk get team %s", ids)
    await asyncio.sleep(0.5)
    source = data["teams"]
    return {id: source[id] for id in ids if id in source}


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
            return await fut

        async def _get_user(user_id):
            fut = asyncio.Future()
            await q1.put((user_id, fut))
            return await fut

        # ここで直接awaitしてしまうと死ぬ
        team_ids = await list_teams()
        for team_id in team_ids:
            team = await _get_team(team_id)
            sd = d[team["name"]] = {"name": team["name"], "members": []}

            futs = []
            for member_id in team["members"]:
                futs.append(_get_user(member_id))

            for member in await asyncio.gather(*futs):
                sd["members"].append({"name": member["name"]})

        logger.info("... end")
        for i, sd in enumerate(d.values()):
            print({"i": i, "data": sd})


def main():
    logging.basicConfig(
        level=logging.INFO, format="%(relativeCreated)d	" + logging.BASIC_FORMAT
    )
    asyncio.run(run())


main()
