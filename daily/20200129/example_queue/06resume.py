import typing as t
import typing_extensions as tx
import queue
import pickle
import pathlib
import time
import contextlib

T = t.TypeVar("T")


class _QueueLike(t.Generic[T], tx.Protocol):
    queue: t.Deque[T]  # undocumented

    def get(self, block: bool = ..., timeout: t.Optional[float] = ...) -> T:
        ...

    def get_nowait(self) -> T:
        ...

    def put(self, item: T, block: bool = ..., timeout: t.Optional[float] = ...) -> None:
        ...

    def put_nowait(self, item: T) -> None:
        ...

    def join(self) -> None:
        ...

    def qsize(self) -> int:
        ...

    def empty(self) -> bool:
        ...

    def task_done(self) -> None:
        ...


class QueueLike(_QueueLike[T]):
    # need:
    latest: t.Optional[T]


def save_queue(q: QueueLike[t.Optional[T]], *, path: pathlib.Path) -> None:
    if not q.empty():
        if path.exists():
            path.unlink()
        with path.open("wb") as wf:
            pickle.dump(q.queue, wf)


def load_queue(
    q: QueueLike[t.Optional[T]], *, path: pathlib.Path
) -> QueueLike[t.Optional[T]]:
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
    q: QueueLike[t.Optional[T]], *, path: pathlib.Path, only: bool = False
) -> t.Iterator[QueueLike[t.Optional[T]]]:
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


def iter_queue(q: QueueLike[t.Optional[T]]) -> t.Iterable[T]:
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

    q: QueueLike[t.Optional[str]] = queue.Queue()  # type:ignore
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
