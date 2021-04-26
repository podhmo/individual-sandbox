import boto3
import asyncio
import logging
import itertools
import sys
from functools import partial

logger = logging.getLogger(__name__)
debug = True
logging.basicConfig(level=logging.INFO)


q = asyncio.Queue()
loop = asyncio.get_event_loop()
ev = asyncio.Event()


class Stage:
    fetchClusters = "fetchClusters"
    fetchServices = "fetchServices"
    describeServices = "describeServices"


env = "develop"


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

        if action == Stage.fetchClusters:
            clusters = await loop.run_in_executor(None, partial(fetch_clusters, client))
            q.task_done()
            for c in clusters:
                await q.put((Stage.fetchServices, (c, None)))
        elif action == Stage.fetchServices:
            cluster, next_token = args

            services, next_token = await loop.run_in_executor(
                None, partial(list_services, client, cluster, next_token=next_token)
            )
            q.task_done()
            if next_token is not None:
                await q.put((Stage.fetchServices, (cluster, next_token)))
            await q.put((Stage.describeServices, (cluster, services)))
        elif action == Stage.describeServices:
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
                is_ok = s["desiredCount"] == s["runningCount"]
                if is_ok:
                    s.pop("events")
                s["prefix"] = "O " if is_ok else "X "
                r.append(s)
            q.task_done()
            if q.empty():
                break
        else:
            raise RuntimeError(f"unexpected action {action}")
    await q.join()
    return r


def _arn_to_name(arn) -> str:
    return arn.rsplit("/", 1)[-1]


def _chunk(iterable, n):
    it = iter(iterable)
    while True:
        chunk_it = itertools.islice(it, n)
        try:
            first_el = next(chunk_it)
        except StopIteration:
            return
        yield tuple(itertools.chain((first_el,), chunk_it))


def fetch_clusters(client):
    return [_arn_to_name(arn) for arn in client.list_clusters()["clusterArns"]]


def list_services(client, cluster, *, next_token=None):
    if next_token is None:
        next_token = ""
    res = client.list_services(cluster=cluster, maxResults=100, nextToken=next_token)
    services = [_arn_to_name(arn) for arn in res["serviceArns"]]
    return services, res.get("nextToken")


def describe_services(client, cluster, *, services):
    res = client.describe_services(cluster=cluster, services=services)
    r = []
    for s in res["services"]:
        r.append(
            {
                "name": s["serviceName"],
                "desiredCount": s["desiredCount"],
                "runningCount": s["runningCount"],
                "events": s["events"],
            }
        )
    return r


q.put_nowait((Stage.fetchClusters, None))
ev.set()
loop.set_debug(debug)
client = boto3.client("ecs")
res = loop.run_until_complete(worker(client))
for s in sorted(res, key=lambda s: s["name"]):
    if s["prefix"] == "O ":
        continue
    print(f"""{s["prefix"]} {s["name"]} ({s["runningCount"]} / {s["desiredCount"]})""")
