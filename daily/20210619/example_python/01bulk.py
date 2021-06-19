import asyncio
import logging

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


async def get_user(id: int):
    logger.info("get user %d", id)
    await asyncio.sleep(0.5)
    return data["users"][id]


async def get_team(id: int):
    logger.info("get team %d", id)
    await asyncio.sleep(0.5)
    return data["teams"][id]


async def list_teams():
    logger.info("list team")
    await asyncio.sleep(0.5)
    return list(data["teams"].keys())


async def run():
    logger.info("start ...")
    d = {}

    team_ids = await list_teams()
    for team_id in team_ids:
        team = await get_team(team_id)
        sd = d[team["name"]] = {"name": team["name"], "members": []}

        futs = []
        for member_id in team["members"]:
            futs.append(get_user(member_id))

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
