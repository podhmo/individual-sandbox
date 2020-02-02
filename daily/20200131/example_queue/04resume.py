from __future__ import annotations
import typing as t
import typing_extensions as tx
import queue
import pickle
import pathlib
import time
import contextlib

T = t.TypeVar("T")


class Queue(t.Generic[T]):
    q: queue.Queue[t.Optional[T]]
    latest: t.Optional[T]

    def __init__(self) -> None:
        self.q = queue.Queue()
        self.latest = None

    def put(self, item: t.Optional[T]) -> None:
        self.q.put(item)

    def put_nowait(self, item: t.Optional[T]) -> None:
        self.q.put_nowait(item)

    def get(self) -> t.Optional[T]:
        item = self.q.get()
        self.latest = item
        return item

    def join(self) -> None:
        self.q.join()

    def task_done(self) -> None:
        self.q.task_done()

    def empty(self) -> bool:
        return self.q.empty()

    @property
    def queue(self) -> t.Deque[t.Optional[T]]:
        return self.q.queue


def save_queue(q: Queue[T], *, path: pathlib.Path) -> None:
    if not q.empty():
        if path.exists():
            path.unlink()
        with path.open("wb") as wf:
            pickle.dump(q.queue, wf)


def load_queue(q: Queue[T], *, path: pathlib.Path) -> Queue[T]:
    if path.exists():
        with path.open("rb") as rf:
            items = pickle.load(rf)
        for item in items:
            if item is None:
                continue
            q.put_nowait(item)
    return q


@contextlib.contextmanager
def resume(
    q: Queue[T], *, path: pathlib.Path, only: bool = False
) -> t.Iterator[Queue[T]]:
    load_queue(q, path=path)
    if only:
        q.put_nowait(None)
    try:
        yield q
        if path.exists():
            path.unlink()
    except KeyboardInterrupt:
        latest = q.latest
        if latest is not None:
            q.queue.appendleft(latest)
        save_queue(q, path=path)


def iter_queue(q: Queue[T]) -> t.Iterable[T]:
    while True:
        item = None
        item = q.latest = q.get()
        if item is None:
            q.task_done()
            break
        yield item
        q.task_done()


def run() -> None:
    def use(itr: t.Iterable[str]) -> None:
        for item in itr:
            print(item)
            time.sleep(0.2)

    path = pathlib.Path(__file__).with_suffix(".pickle")

    q: Queue[str] = Queue()
    q.latest = None

    if path.exists():
        with resume(q, path=path, only=True) as q:
            use(iter_queue(q))
    else:
        for i in range(5):
            q.put_nowait(str(i))
        q.put_nowait(None)
        with resume(q, path=path) as q:
            use(iter_queue(q))


run()
