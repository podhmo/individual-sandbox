import typing as t
import queue


class Protocol:
    def encode(self, v: t.Any, **kawrgs: t.Any):
        return v

    def decode(self, v: t.Any):
        return v

    # task done?


def produce(
    p: Protocol, q, itr: t.Iterator[int], *, teardown: t.Callable[[t.Any], None]=None
) -> queue.Queue:
    try:
        for i in itr:
            q.put(p.encode(i))
    finally:
        if teardown is not None:
            teardown(q)


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
    p = Identity()
    q: queue.Queue[t.Optional[int]] = queue.Queue()
    produce(p, q, range(5), teardown=lambda q: q.put(None))
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

    p = Protocol(Identity())
    q: queue.PriorityQueue[t.Optional[int]] = queue.PriorityQueue()
    q.put((math.inf, None))
    produce(p, q, range(5))
    consume(p, q, prefix=prefix)


def run_lifo(prefix: str) -> None:
    p = Identity()
    q: queue.Queue[t.Optional[int]] = queue.LifoQueue()
    q.put(None)
    produce(p, q, range(5))
    consume(p, q, prefix=prefix)


run("Queue")
run_priority("PriorityQueue")
run_lifo("LIFOQueue")
