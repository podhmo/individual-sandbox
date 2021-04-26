import typing as t
import os
import inspect
import asyncio
import logging
import itertools
import sys
from collections import namedtuple

logger = logging.getLogger(__name__)
END = None
ActionItem = namedtuple("ActionItem", "action, args, kwargs")


@t.runtime_checkable
class APIClient(t.Protocol):
    async def list_clusters(self) -> t.List[str]:
        ...

    async def list_services(
        self, cluster: str, *, next_token: t.Optional[str] = None
    ) -> t.Tuple[t.List[str], t.Optional[str]]:
        ...

    async def describe_services(
        self, cluster: str, *, services: t.List[str]
    ) -> t.List[t.Dict[str, t.Any]]:
        ...


@t.runtime_checkable
class ActionQueue(t.Protocol):
    async def add_next_action(
        self,
        action: t.Callable[..., t.Awaitable[t.Optional[bool]]],
        *args: t.Any,
        **kwargs: t.Any,
    ) -> None:
        ...

    async def run(self) -> None:
        ...


# implements APIClient
class _MockClient:
    async def list_clusters(self) -> t.List[str]:
        await asyncio.sleep(0.5)
        return ["app", "spot-batch"]

    async def list_services(
        self, cluster: str, *, next_token: t.Optional[str] = None
    ) -> t.Tuple[t.List[str], t.Optional[str]]:
        await asyncio.sleep(0.5)
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

    async def describe_services(
        self, cluster: str, *, services: t.List[str]
    ) -> t.List[t.Dict[str, t.Any]]:
        await asyncio.sleep(0.5)
        r = []
        for name in services:
            assert isinstance(name, str), name
            r.append(
                {"name": name, "desiredCount": 1, "runningCount": 1, "prefix": "O "}
            )
        return r


class ActionHandler:
    def __init__(
        self,
        client: APIClient,
        results: t.List[t.Dict[str, t.Any]],
        *,
        aq: ActionQueue,
    ) -> None:
        self.results = results
        self.client = client
        self.aq = aq

    async def do_list_clusters(self) -> t.Optional[bool]:
        clusters = await self.client.list_clusters()
        for c in clusters:
            await self.aq.add_next_action(
                self.do_list_services, cluster=c, next_token=None
            )
        return None

    async def do_list_services(
        self, cluster: str, next_token: t.Optional[str],
    ) -> t.Optional[bool]:
        services, new_next_token = await self.client.list_services(
            cluster, next_token=next_token
        )
        if new_next_token is not None:
            await self.aq.add_next_action(
                self.do_list_services, cluster=cluster, next_token=new_next_token
            )
        for parts in _chunk(services, n=10):
            await self.aq.add_next_action(
                self.do_describe_services, cluster=cluster, services=parts
            )
        return None

    async def do_describe_services(
        self, cluster: str, services: t.List[str],
    ) -> t.Optional[bool]:
        res = await self.client.describe_services(cluster, services=services)
        await self.aq.add_next_action(self.do_end, cluster=cluster, services=res)
        return None

    async def do_end(
        self, cluster: str, services: t.List[t.Dict[str, t.Any]]
    ) -> t.Optional[bool]:
        for s in services:
            self.results.append(s)
        return True  # last action


class BulkActionQueue:
    def __init__(
        self,
        q: asyncio.Queue[t.Optional[ActionItem]],
        *,
        debug: bool = False,
        buffering_time: float = 0.1,
        max_items: int = -1,
    ):
        self.q = q
        self.debug = debug
        self.buffering_time = buffering_time
        self.max_items = max_items

    async def add_next_action(
        self, action: t.Callable[..., t.Awaitable[t.Any]], *args: t.Any, **kwargs: t.Any
    ) -> None:
        inspect.getcallargs(action, *args, **kwargs)
        await self.q.put(ActionItem(action=action, args=args, kwargs=kwargs))

    async def bulk_get_action(self) -> t.Tuple[t.List[ActionItem], bool]:
        q = self.q

        items: t.List[ActionItem] = []
        is_end = False

        async def _bulk_get(*, first: bool) -> None:
            nonlocal items
            nonlocal is_end

            while True:
                item = await q.get()
                if item is END:
                    q.task_done()
                    is_end = len(items) == 0
                    return

                if self.debug:
                    logger.info("action %s -- %r", item.action.__name__, "")  # )args)
                else:
                    print(".", file=sys.stderr, end="")
                    sys.stderr.flush()

                items.append(item)
                if first:
                    break
                if self.max_items > 0 and len(items) >= self.max_items:
                    return

        try:
            await _bulk_get(first=True)
            await asyncio.wait_for(_bulk_get(first=False), self.buffering_time)
        except asyncio.TimeoutError:
            pass

        return items, is_end

    async def run(self) -> None:
        q = self.q

        while True:
            items, is_end = await self.bulk_get_action()
            if is_end:
                if q.empty():
                    break
                continue

            if len(items) == 0:
                continue

            # print("@", len(items))
            has_end = False
            futs = [action(*args, **kwargs) for action, args, kwargs in items]
            for is_end_or_exc in await asyncio.gather(*futs, return_exceptions=True):
                q.task_done()
                if is_end_or_exc is True:
                    has_end = True
                elif is_end_or_exc is not None:
                    raise is_end_or_exc

            if has_end:
                await q.put(END)

        await q.join()
        assert q.empty(), q


def _chunk(iterable: t.Iterable[t.Any], *, n: int) -> t.Iterable[t.Tuple[t.Any, ...]]:
    it = iter(iterable)
    while True:
        chunk_it = itertools.islice(it, n)
        try:
            first_el = next(chunk_it)
        except StopIteration:
            return
        yield tuple(itertools.chain((first_el,), chunk_it))


def run(*, env: t.Optional[str] = None) -> None:
    debug = True
    lv_level = logging.DEBUG if debug else logging.INFO
    logging.basicConfig(
        level=lv_level, format="%(relativeCreated)-10d" + logging.BASIC_FORMAT
    )

    async def inner() -> t.List[t.Dict[str, t.Any]]:
        results: t.List[t.Dict[str, t.Any]] = []
        q: asyncio.Queue[t.Optional[ActionItem]] = asyncio.Queue()

        client = _MockClient()
        assert isinstance(client, APIClient)

        aq = BulkActionQueue(q, debug=debug)
        ah = ActionHandler(client, results, aq=aq)
        await aq.add_next_action(ah.do_list_clusters)

        await aq.run()
        return results

    r = asyncio.run(inner(), debug=debug)
    print()
    for s in sorted(r, key=lambda s: s["name"]):
        # if s["prefix"] == "O ":
        #     continue
        print(
            f"""{s["prefix"]} {s["name"]} ({s["runningCount"]} / {s["desiredCount"]})"""
        )


def main(argv: t.Optional[t.List[str]] = None) -> t.Any:
    import argparse

    parser = argparse.ArgumentParser(
        prog=run.__name__,
        description=run.__doc__,
        formatter_class=type(
            "_HelpFormatter",
            (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter),
            {},
        ),
    )
    parser.print_usage = parser.print_help  # type: ignore
    parser.add_argument("--env", help="-")
    args = parser.parse_args(argv)
    params = vars(args).copy()
    action = run
    if bool(os.getenv("FAKE_CALL")):
        from inspect import getcallargs
        from functools import partial

        action = partial(getcallargs, action)  # type: ignore
    return action(**params)


if __name__ == "__main__":
    main()
