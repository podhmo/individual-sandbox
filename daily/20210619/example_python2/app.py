import typing as t
from collections import defaultdict
import asyncio
import logging
from starlette.applications import Starlette
from starlette.responses import JSONResponse
from starlette.routing import Route

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


class NotFound(Exception):
    pass


async def create_data_loader(
    q: asyncio.Queue[t.Tuple[int, asyncio.Future]],
    bulk_get,
    *,
    buffering_time: float = 0.1
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
                logger.debug("cancel id %d, in data loader %s", unseen_id, bulk_get)
                for fut in waiter_dict[unseen_id]:
                    fut.set_exception(NotFound(unseen_id))

    except Exception as e:
        logger.exception(e)
    finally:
        logger.info("stop data lodader %s", bulk_get)


User = t.Dict[t.Any, t.Any]
Team = t.Dict[t.Any, t.Any]


async def bulk_get_user(ids: t.Sequence[int]) -> t.Dict[int, User]:
    logger.info("bulk get user %s", ids)
    await asyncio.sleep(0.5)
    source = data["users"]
    r = {id: source[id] for id in ids if id in source}
    logger.debug("bulk get user %s ... ok", ids)
    return r


async def bulk_get_team(ids: t.Sequence[int]) -> t.Dict[int, Team]:
    logger.info("bulk get team %s", ids)
    await asyncio.sleep(0.5)
    source = data["teams"]
    r = {id: source[id] for id in ids if id in source}
    logger.debug("bulk get team %s ... ok", ids)
    return r


q0 = asyncio.Queue()
q1 = asyncio.Queue()


async def _get_team(team_id: int) -> Team:
    fut = asyncio.Future()
    await q0.put((team_id, fut))
    r = await fut
    logger.debug("wait team %s -- %s", team_id, r)
    return r


async def _get_user(user_id: int) -> User:
    fut = asyncio.Future()
    await q1.put((user_id, fut))
    r = await fut
    logger.debug("wait user %s -- %s", user_id, r)
    return r


clean_up_functions = []


async def start_data_loader():
    global clean_up_functions
    loop = asyncio.get_event_loop()

    t0 = loop.create_task(create_data_loader(q0, bulk_get_team))
    clean_up_functions.append(t0.cancel)
    t1 = loop.create_task(create_data_loader(q1, bulk_get_user))
    clean_up_functions.append(t1.cancel)


async def stop_data_loader():
    global clean_up_functions
    for fn in clean_up_functions:
        fn()
    # print(asyncio.all_tasks())


async def team_fullset(request):
    team_id = request.path_params["team_id"]
    try:
        team = await _get_team(team_id)
    except NotFound:
        return JSONResponse({"not found": team_id}, status_code=404)

    d = {"name": team["name"], "members": []}

    futs = []
    for member_id in team["members"]:
        futs.append(_get_user(member_id))
    for member_or_error in await asyncio.gather(*futs, return_exceptions=True):
        if isinstance(member_or_error, Exception):
            if not isinstance(member_or_error, NotFound):
                raise member_or_error
        else:
            d["members"].append({"name": member_or_error["name"]})
    return JSONResponse(d)


logging.basicConfig(
    # level=logging.DEBUG, format="%(relativeCreated)d	" + logging.BASIC_FORMAT
    level=logging.INFO,
    format="%(relativeCreated)d	" + logging.BASIC_FORMAT,
)


routes = [Route("/teams/{team_id:int}", team_fullset)]
app = Starlette(
    routes=routes, on_startup=[start_data_loader], on_shutdown=[stop_data_loader]
)

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, debug=False)
