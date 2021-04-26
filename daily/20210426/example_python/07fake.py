import asyncio
import logging
import itertools
import sys
import time
from functools import partial

logger = logging.getLogger(__name__)
debug = False
logging.basicConfig(
    level=logging.INFO, format="%(relativeCreated)-10d" + logging.BASIC_FORMAT
)


q = asyncio.Queue()
loop = asyncio.get_event_loop()
ev = asyncio.Event()
END = None


async def bulk_get(q, *, buffering_time=0.1, max_items=-1):
    items = []
    is_end = False

    async def _bulk_get(*, first: bool):
        nonlocal items
        nonlocal is_end

        while True:
            item = await q.get()
            if item is END:
                q.task_done()
                is_end = len(items) == 0
                return

            afn, args = item
            if debug:
                logger.info("afn %s -- %r", afn, "")  # )args)
            else:
                print(".", file=sys.stderr, end="")
                sys.stderr.flush()

            items.append((afn, args))
            if first:
                break
            if max_items > 0 and len(items) >= max_items:
                return

    try:
        await _bulk_get(first=True)
        await asyncio.wait_for(_bulk_get(first=False), buffering_time)
    except asyncio.TimeoutError:
        pass

    return items, is_end


async def worker():
    logger.info("init")
    await ev.wait()
    logger.info("start")

    while True:
        futs = []
        items, is_end = await bulk_get(q, buffering_time=0.1)
        if is_end:
            if q.empty():
                break
            continue

        if len(items) == 0:
            continue

        # print("@", len(items))
        for afn, args in items:
            futs.append(afn(*args))

        has_end = False
        for is_end_or_exc in await asyncio.gather(*futs, return_exceptions=True):
            q.task_done()
            if is_end_or_exc is True:
                has_end = True
            elif is_end_or_exc is not None:
                raise is_end_or_exc

        if has_end:
            await q.put(END)

    await q.join()


async def simple_worker():
    logger.info("init")
    await ev.wait()
    logger.info("start")

    while True:
        item = await q.get()
        if item is END:
            q.task_done()
            if q.empty():
                break
            continue

        afn, args = item
        if debug:
            logger.info("afn %s -- %r", afn, "")  # )args)
        else:
            print(".", file=sys.stderr, end="")
            sys.stderr.flush()

        has_end = await afn(*args)
        q.task_done()
        if has_end:
            await q.put(END)
    await q.join()


def _chunk(iterable, n):
    it = iter(iterable)
    while True:
        chunk_it = itertools.islice(it, n)
        try:
            first_el = next(chunk_it)
        except StopIteration:
            return
        yield tuple(itertools.chain((first_el,), chunk_it))


async def aggressive_worker():
    logger.info("init")
    await ev.wait()
    logger.info("start")

    prev_pendings = []
    while True:
        while True:
            futs = []
            items, is_end = await bulk_get(q, buffering_time=0.1)
            if is_end:
                if q.empty():
                    break
                continue

            for afn, args in items:
                futs.append(loop.create_task(afn(*args)))

            # print(
            #     "->",
            #     "unfinished",
            #     q._unfinished_tasks,
            #     "items",
            #     len(items),
            #     "prev_pending",
            #     len(prev_pendings),
            # )
            futs.extend(prev_pendings)
            prev_pendings = []

            if len(futs) == 0:
                continue

            done, pending = await asyncio.wait(futs, timeout=0.4)
            # print(
            #     "<-",
            #     "unfinished",
            #     q._unfinished_tasks,
            #     "done",
            #     len(done),
            #     "pending",
            #     len(pending),
            # )
            if len(pending) > 0:
                prev_pendings = list(pending)

            has_end = False
            for fut in done:
                is_end = fut.result()
                q.task_done()
                if is_end is True:
                    has_end = True
            # print(
            #     "--",
            #     "unfinished",
            #     q._unfinished_tasks,
            #     "prev_pending",
            #     len(prev_pendings),
            # )
            if has_end:
                await q.put(END)

        if len(prev_pendings) > 0:
            st = time.time()
            await asyncio.wait(prev_pendings)
            print(time.time() - st)
            # print("..", q)
        if q.empty():
            await q.join()
            break


class Client:
    def __init__(self, client):
        self.client = client

    def list_clusters(self):
        time.sleep(0.5)
        return ["app", "spot-batch"]

    def list_services(self, cluster, *, next_token=None):
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

    def describe_services(self, cluster, *, services):
        time.sleep(0.5)
        r = []
        for name in services:
            assert isinstance(name, str), name
            r.append(
                {"name": name, "desiredCount": 1, "runningCount": 1, "prefix": "O "}
            )
        return r


class Executor:
    def __init__(self, r, *, q, loop):
        self.r = r
        self.q = q
        self.loop = loop

    async def do_list_clusters(self, client):
        clusters = await self.loop.run_in_executor(None, client.list_clusters)
        for c in clusters:
            await self.q.put((self.do_list_services, (client, c, None)))

    async def do_list_services(self, client, cluster, next_token):
        services, new_next_token = await self.loop.run_in_executor(
            None, partial(client.list_services, cluster, next_token=next_token),
        )
        if new_next_token is not None:
            await self.q.put((self.do_list_services, (client, cluster, new_next_token)))
        for parts in _chunk(services, 10):
            await self.q.put((self.do_describe_services, (client, cluster, parts)))

    async def do_describe_services(self, client, cluster, services):
        res = await self.loop.run_in_executor(
            None, partial(client.describe_services, cluster, services=services),
        )
        await self.q.put((self.do_end, (client, cluster, res)))

    async def do_end(self, client, cluster, services):
        for s in services:
            self.r.append(s)
        return True  # has end


r = []
client = Client(None)
ex = Executor(r, loop=loop, q=q)
q.put_nowait((ex.do_list_clusters, [client]))
ev.set()
loop.set_debug(debug)
# loop.run_until_complete(simple_worker())
loop.run_until_complete(worker())
# loop.run_until_complete(aggressive_worker())
for s in sorted(r, key=lambda s: s["name"]):
    # if s["prefix"] == "O ":
    #     continue
    print(f"""{s["prefix"]} {s["name"]} ({s["runningCount"]} / {s["desiredCount"]})""")
