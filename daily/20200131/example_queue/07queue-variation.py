import typing as t
import queue


def run(prefix: str) -> None:
    # provide
    q: queue.Queue[t.Optional[int]] = queue.Queue()
    for i in range(5):
        q.put(i)
    q.put(None)

    # consume
    while True:
        item = q.get()
        if item is None:
            q.task_done()
            break
        print(f"{prefix:<10}	use:{item}	raw: {item}")
        q.task_done()


def run_priority(prefix: str) -> None:
    import random
    import math

    # provide
    q: queue.PriorityQueue[t.Tuple[float, t.Optional[int]]] = queue.PriorityQueue()
    for i in range(5):
        q.put((random.random(), i))
    q.put((math.inf, None))

    # consume
    while True:
        priority, item = q.get()
        if item is None:
            q.task_done()
            break
        print(f"{prefix:<10}	use:{item}	raw: {(priority, item)}")
        q.task_done()


def run_lifo(prefix: str) -> None:
    # provide
    q: queue.LifoQueue[t.Optional[int]] = queue.LifoQueue()
    q.put(None)
    for i in range(5):
        q.put(i)

    # consume
    while True:
        item = q.get()
        if item is None:
            q.task_done()
            break
        print(f"{prefix:<10}	use:{item}	raw: {item}")
        q.task_done()


run("Queue")
run_priority("PriorityQueue")
run_lifo("LIFOQueue")
