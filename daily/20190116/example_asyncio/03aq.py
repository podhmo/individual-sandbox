import asyncio
import logging
logger = logging.getLogger(__name__)


class IDGen:
    def __init__(self):
        self.i = 0

    def __call__(self):
        self.i += 1
        return self.i


class AQ:
    # todo: custom q
    # todo: delay time
    # todo: keyboard interrupt

    def __init__(self, loop, *, idgen=None):
        self.loop = loop
        self.q = asyncio.Queue()  # xxx
        self._workers = []
        self._idgen = idgen or IDGen()
        self._conts = {}
        self._started = False

    async def _worker(self):
        while True:
            i, afn, args, kwargs = await self.q.get()
            logger.debug("get id=%s, afn=%s", i, afn)
            try:
                result = await afn(*args, **kwargs)
                self._conts.pop(i).set_result(result)
            except Exception as e:
                self._conts.pop(i).set_exception(e)
            finally:
                self.q.task_done()

    def add(self, afn, *args, **kwargs):
        i = self._idgen()
        logger.debug("put id=%s, afn=%s", i, afn)
        self.q.put_nowait((i, afn, args, kwargs))
        fut = self._conts[i] = self.loop.create_future()
        return fut

    def start(self, n, *, worker=None):
        self._started = True
        logger.debug("start (max worker=%d)", n)
        for i in range(min(self.q.qsize(), n)):
            t = worker or self._worker()
            self._workers.append(self.loop.create_task(t))

    def join(self):  # todo: timeout, todo: stop
        if not self._started:
            raise RuntimeError("start() is not called, please calling this method, before join()")

        async def _run():
            await self.q.join()

            while not self.q.empty():
                await asyncio.sleep(0.1, loop=self.loop)
                if not self.q.empty():
                    await self.q.join()

            for w in self._workers:
                w.cancel()

        return self.loop.run_until_complete(_run())


def main():
    loop = asyncio.get_event_loop()
    aq = AQ(loop)

    import random

    async def task(item, i):
        d, n = random.random(), random.random()
        print(f"{item:02}:{i}	d={d:.5f}	n={n:.5f}")
        await asyncio.sleep(d)

        if n > 0.75:
            await aq.add(task, item, i + 1)

    for item in range(3):
        aq.add(task, item, 0)

    aq.start(2)
    aq.join()


if __name__ == "__main__":
    main()
