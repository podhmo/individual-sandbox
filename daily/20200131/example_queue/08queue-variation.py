import typing as t
import queue


class Protocol:
    def on_startup(self, q):
        pass

    def on_shutdown(self, q):
        pass

    def encode(self, v: t.Any, **kawrgs: t.Any):
        return v

    def decode(self, v: t.Any):
        return v

    # task done?
    # create queue?


def produce(p: Protocol, q, itr: t.Iterator[int]) -> queue.Queue:
    p.on_startup(q)
    for i in itr:
        q.put(p.encode(i))
    p.on_shutdown(q)


def consume(p: Protocol, q, *, prefix: str):
    while True:
        raw = q.get()
        item = p.decode(raw)
        if item is None:
            q.task_done()
            break
        print(f"{prefix:<10}	use:{item}	raw: {raw}")
        q.task_done()


class Identity:
    def encode(self, v: t.Any, **kawrgs: t.Any):
        return v

    def decode(self, v: t.Any):
        return v


def run(prefix: str) -> None:
    class Protocol(Identity):
        def on_startup(self, q):
            pass

        def on_shutdown(self, q):
            q.put(None)

    p = Protocol()
    q: queue.Queue[t.Optional[int]] = queue.Queue()
    produce(p, q, range(5))
    consume(p, q, prefix=prefix)


def run_priority(prefix: str) -> None:
    import random
    import math

    class Protocol:
        def __init__(self, internal):
            self.internal = internal

        def encode(self, v: t.Any, **kawrgs: t.Any):
            return (random.random(), self.internal.encode(v))

        def decode(self, v: t.Any):
            return self.internal.decode(v[1])

        def on_startup(self, q):
            pass

        def on_shutdown(self, q):
            q.put((math.inf, None))

    p = Protocol(Identity())
    q: queue.Queue[t.Optional[int]] = queue.Queue()
    produce(p, q, range(5))
    consume(p, q, prefix=prefix)


def run_lifo(prefix: str) -> None:
    class Protocol(Identity):
        def on_startup(self, q):
            q.put(None)

        def on_shutdown(self, q):
            pass

    p = Protocol()
    q: queue.Queue[t.Optional[int]] = queue.LifoQueue()
    produce(p, q, range(5))
    consume(p, q, prefix=prefix)


run("Queue")
run_priority("PriorityQueue")
run_lifo("LIFOQueue")
