import typing as t
import sys
import queue
import pickle
import pathlib
import time
import contextlib


def save_queue(q: queue.Queue, *, path: pathlib.Path):
    if not q.empty():
        print("save", file=sys.stderr)
        if path.exists():
            path.unlink()
        with path.open("wb") as wf:
            pickle.dump(q.queue, wf)


def load_queue(q: queue.Queue, *, path: pathlib.Path):
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
    q: t.Optional[queue.Queue] = None, *, path: pathlib.Path, only: bool = False
) -> queue.Queue:
    q = q or queue.Queue()
    load_queue(q, path=path)
    if only:
        q.put_nowait(None)
    try:
        yield q
        if path.exists():
            path.unlink()
    except KeyboardInterrupt:
        if q.latest is not None:
            q.queue.appendleft(q.latest)
        save_queue(q, path=path)


class Interrupted(KeyboardInterrupt):
    def __init__(self, *, item):
        self.item = item


def iter_queue(q: queue.Queue):
    while True:
        item = None
        item = q.get()
        q.latest = item  # xxx
        if item is None:
            q.task_done()
            break
        yield item
        q.task_done()


def run():
    def use(itr: t.Iterable[int]):
        for item in itr:
            print(item)
            time.sleep(0.2)

    path = pathlib.Path(__file__).with_suffix(".pickle")
    if path.exists():
        with resume(path=path, only=True) as q:
            use(iter_queue(q))
    else:
        q = queue.Queue()
        for i in range(5):
            q.put_nowait(i)
        q.put_nowait(None)
        with resume(q, path=path) as q:
            use(iter_queue(q))


run()
