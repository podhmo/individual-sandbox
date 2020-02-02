from __future__ import annotations
import typing as t
import queue

T = t.TypeVar("T")


class Queue(t.Generic[T]):
    q: queue.Queue[t.Optional[T]]
    latest: t.Optional[T]

    def __init__(self) -> None:
        self.q = queue.Queue()
        self.latest = None

    def put(self, item: t.Optional[T]) -> None:
        self.q.put(item)

    def get(self) -> t.Optional[T]:
        item = self.q.get()
        self.latest = item
        return item

    def join(self) -> None:
        self.q.join()

    def task_done(self) -> None:
        self.q.task_done()


q: Queue[int] = Queue()
for i in range(5):
    q.put(i)
q.put(None)

while True:
    item = q.get()
    if item is None:
        q.task_done()
        break
    print(item)
    # do something
    q.task_done()
