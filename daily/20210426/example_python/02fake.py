import asyncio
import logging
import itertools
import sys
import time
from functools import partial

logger = logging.getLogger(__name__)
debug = True
logging.basicConfig(
    level=logging.INFO, format="%(relativeCreated)-10d" + logging.BASIC_FORMAT
)


q = asyncio.Queue()
loop = asyncio.get_event_loop()
ev = asyncio.Event()


async def worker(client):
    logger.info("init")
    await ev.wait()
    logger.info("start")

    r = []
    while True:
        (action, args) = await q.get()
        if debug:
            logger.info("action %s -- %r", action, "")  # )args)
        else:
            print(".", file=sys.stderr, end="")
            sys.stderr.flush()

        if action == list_clusters:
            clusters = await loop.run_in_executor(None, partial(list_clusters, client))
            q.task_done()
            for c in clusters:
                await q.put((list_services, (c, None)))
        elif action == list_services:
            cluster, next_token = args

            services, next_token = await loop.run_in_executor(
                None, partial(list_services, client, cluster, next_token=next_token)
            )
            q.task_done()
            if next_token is not None:
                await q.put((list_services, (cluster, next_token)))
            await q.put((describe_services, (cluster, services)))
        elif action == describe_services:
            cluster, services = args

            futs = []
            for parts in _chunk(services, 10):
                futs.append(
                    loop.run_in_executor(
                        None,
                        partial(describe_services, client, cluster, services=parts),
                    )
                )
            for res in await asyncio.gather(*futs):
                await q.put((None, (cluster, res)))
            q.task_done()

        elif action is None:  # end
            cluster, services = args
            for s in services:
                r.append(s)
            q.task_done()
            if q.empty():
                break
        else:
            raise RuntimeError(f"unexpected action {action}")
    await q.join()
    return r


def _chunk(iterable, n):
    it = iter(iterable)
    while True:
        chunk_it = itertools.islice(it, n)
        try:
            first_el = next(chunk_it)
        except StopIteration:
            return
        yield tuple(itertools.chain((first_el,), chunk_it))


def list_clusters(client):
    time.sleep(0.5)
    return ["app", "spot-batch"]


def list_services(client, cluster, *, next_token=None):
    time.sleep(0.5)
    if cluster == "app":
        if next_token is None:
            return [f"app{i:02d}" for i in range(20)], "next_token_00"
        elif next_token == "next_token_00":
            return [f"app{i:02d}" for i in range(20, 40)], "next_token_01"
        else:
            return [f"app{i:02d}" for i in range(20, 60)], None
    elif cluster == "spot-batch":
        if next_token is None:
            return [f"spot-batch{i:02d}" for i in range(20)], "next_token_00"
        elif next_token == "next_token_00":
            return [f"spot-batch{i:02d}" for i in range(20, 40)], "next_token_01"
        elif next_token == "next_token_01":
            return [f"spot-batch{i:02d}" for i in range(40, 60)], "next_token_02"
        else:
            return [f"spot-batch{i:02d}" for i in range(60, 80)], None
    else:
        raise NotImplementedError(f"unexpected cluster, {cluster}")


def describe_services(client, cluster, *, services):
    time.sleep(0.5)
    r = []
    for name in services:
        r.append({"name": name, "desiredCount": 1, "runningCount": 1, "prefix": "O "})
    return r


q.put_nowait((list_clusters, None))
ev.set()
loop.set_debug(debug)
client = None
res = loop.run_until_complete(worker(client))
for s in sorted(res, key=lambda s: s["name"]):
    # if s["prefix"] == "O ":
    #     continue
    print(f"""{s["prefix"]} {s["name"]} ({s["runningCount"]} / {s["desiredCount"]})""")
