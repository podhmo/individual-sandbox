import typing as t
import typing_extensions as tx
import queue


class Protocol(tx.Protocol):
    def encode(self, v: t.Any, **kawrgs: t.Any):
        ...

    def decode(self, v: t.Any):
        ...

    def create_queue(self) -> t.Tuple[t.Any, t.Callable]:
        ...

    # task done?


def produce(p: Protocol, itr: t.Iterator[int]) -> queue.Queue:
    q, teardown = p.create_queue()
    try:
        for i in itr:
            q.put(p.encode(i))
    finally:
        if teardown is not None:
            teardown(q)
    return q


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
        def create_queue(self):
            q = queue.Queue()
            return q, lambda q: q.put(None)

    p = Protocol()
    q = produce(p, range(5))
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

        def create_queue(self):
            q: queue.PriorityQueue[t.Optional[int]] = queue.PriorityQueue()
            q.put((math.inf, None))
            return q, None

    p = Protocol(Identity())
    q = produce(p, range(5))
    consume(p, q, prefix=prefix)


def run_lifo(prefix: str) -> None:
    class Protocol(Identity):
        def create_queue(self):
            q: queue.Queue[t.Optional[int]] = queue.LifoQueue()
            q.put(None)
            return q, None

    p = Protocol()
    q = produce(p, range(5))
    consume(p, q, prefix=prefix)


run("Queue")
run_priority("PriorityQueue")
run_lifo("LIFOQueue")
