from __future__ import annotations
import typing as t
import asyncio
import logging
import contextlib
from functools import partial
logger = logging.getLogger(__name__)

# xxx:
Id = t.Union[str, int]
Number = t.Union[float, int]


class IDGen:
    def __init__(self) -> None:
        self.i = 0

    def __call__(self) -> Id:
        self.i += 1
        return self.i


async def sleep(n: t.Number, *, loop: asyncio.BaseEventLoop = None) -> None:
    await asyncio.sleep(n, loop=loop)
    logger.debug("**sleep after %s", n)

class AQ:
    # todo: keyboard interrupt
    # todo: graceful shutdown
    # todo: serialize/deserialize (other object's responsibility?)

    loop: asyncio.BaseEventLoop
    q: asyncio.Queue

    def __init__(
        self,
        loop: asyncio.BaseEventLoop,
        *,
        idgen: t.Callable[[], Id] = None,
    ) -> None:
        self.loop = loop
        self.q = q or asyncio.Queue()  # xxx
        self._workers = []
        self._idgen = idgen or IDGen()
        self._conts = {}
        self._started = False

    async def _worker(self, k):
        while True:
            i = None
            try:
                i, afn, args, kwargs = await self.q.get()
                logger.debug("get[%r]	id=%r	 afn=%r", k, i, getattr(afn, "__name__", afn))
                result = await afn(*args, **kwargs)
                if callable(result):
                    logger.debug("continue[%r]	id=%r", k, i)
                    self.add(result, _id=i)  # await?
                else:
                    logger.debug("finish[%r]	id=%r", k, i)
                    self._conts.pop(i).set_result(result)
            except asyncio.CancelledError:
                logger.debug("canceled[%r]", k)
            except Exception as e:
                if i is None:
                    # todo master error handler
                    logger.error("error	%r", e, exc_info=True)
                else:
                    logger.error("error[%r]	%r", i, e, exc_info=True)
                    self._conts.pop(i).set_exception(e)
            finally:
                self.q.task_done()
                logger.debug(
                    "done[%r]	id=%r	 q=(%r, %r))", k, i, self.q.qsize(), self.q._unfinished_tasks
                )

    def add(self, afn, *args, _id=None, **kwargs):
        i = _id or self._idgen()
        logger.debug("put	id=%r	afn=%r", i, getattr(afn, "__name__", afn))
        self.q.put_nowait((i, afn, args, kwargs))

        fut = self._conts.get(i)
        if fut is None:
            fut = self._conts[i] = self.loop.create_future()
        return fut

    def start(self, n, *, worker=None, max_workers=None):
        self._started = True
        max_workers = max_workers or min(self.q.qsize(), n)
        for k in range(max_workers):
            t = worker or self._worker(k)
            logger.debug("start[%r]	(max worker=%d)", k, max_workers)
            self._workers.append(self.loop.create_task(t))

    def join(self):  # todo: timeout, todo: stop
        if not self._started:
            raise RuntimeError("start() is not called, please calling this method, before join()")

        async def _run():
            await self.q.join()
            for w in self._workers:
                w.cancel()

        return self.loop.run_until_complete(_run())


@contextlib.contextmanager
def do_async_tasks(n, loop=None):
    loop = loop or asyncio.get_event_loop()
    aq = AQ(loop)
    yield aq
    aq.start(n)

    async def ping():
        while True:
            logger.debug(
                "	empty?:%s	size:%s	unfinished:%s",
                aq.q.empty(),
                aq.q.qsize(),
                aq.q._unfinished_tasks,
            )
            await asyncio.sleep(0.5, loop=loop)

    loop.create_task(ping())
    aq.join()
    assert aq.q.qsize() == 0, (aq.q.qsize(), 0)
    assert aq.q._unfinished_tasks == 0, aq.q._unfinished_tasks


def main():
    import random

    async def task(item, i):
        d, n = random.random(), random.random()
        logger.info("task d=%r n=%r", d, n)
        print(f"{item:02}:{i}	d={d:.5f}	n={n:.5f}")
        await sleep(d)

        if n > 0.75:
            return partial(task, item, i)  # cont
        if n < 0.2:
            raise Exception("hmm", n)
        return (item, i)

    finished = 0
    N = 20

    with do_async_tasks(5) as aq:
        for item in range(N):
            fut = aq.add(task, item, 0)

            def cont(fut):
                try:
                    nonlocal finished
                    finished += 1
                    logger.info("callback	result=%r	called=%r", fut.result(), finished)
                except Exception as e:
                    logger.error("ERROR %r", e, exc_info=True)

            fut.add_done_callback(cont)
    assert finished == N, finished


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    main()
