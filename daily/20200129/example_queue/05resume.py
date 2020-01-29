import typing as t
import queue
import pickle
import pathlib
import time
import contextlib

if t.TYPE_CHECKING:
    Queue = queue.Queue[t.Optional[int]]
else:
    Queue = queue.Queue


def save_queue(q: Queue, *, path: pathlib.Path) -> None:
    if not q.empty():
        if path.exists():
            path.unlink()
        with path.open("wb") as wf:
            pickle.dump(q.queue, wf)


def load_queue(q: Queue, *, path: pathlib.Path) -> Queue:
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
    q: t.Optional[Queue] = None, *, path: pathlib.Path, only: bool = False
) -> t.Iterator[Queue]:
    q = q or Queue()
    load_queue(q, path=path)
    if only:
        q.put_nowait(None)
    try:
        yield q
        if path.exists():
            path.unlink()
    except KeyboardInterrupt:
        latest = q.latest  # type:ignore
        if latest is not None:
            q.queue.appendleft(latest)
        save_queue(q, path=path)


def iter_queue(q: Queue) -> t.Iterable[int]:
    while True:
        item = None
        item = q.get()
        q.latest = item  # type:ignore
        if item is None:
            q.task_done()
            break
        yield item
        q.task_done()


def run() -> None:
    def use(itr: t.Iterable[int]) -> None:
        for item in itr:
            print(item)
            time.sleep(0.2)

    path = pathlib.Path(__file__).with_suffix(".pickle")
    if path.exists():
        with resume(path=path, only=True) as q:
            use(iter_queue(q))
    else:
        q = Queue()
        for i in range(5):
            q.put_nowait(i)
        q.put_nowait(None)
        with resume(q, path=path) as q:
            use(iter_queue(q))


run()
