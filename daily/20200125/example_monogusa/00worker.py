import multiprocessing
import time
import queue
from functools import partial
from handofcats import as_command

"""

TODO: workerの中で本当はloopしたい(queueがprocessまたいじゃうのでerrorになる)
TODO: もっと手軽に扱いたい。blockを呼ぶ処理を呼んでしまうと後始末ができない？
(daemon=Trueは避けたい)
processを使い続けるということは厳しいかもしれない。結局通信が必要になるのは変わらない。
"""


def _worker(*, uid: int, item) -> None:
    import time

    for i in range(5):
        print(uid, i, item)
        time.sleep(0.1)


@as_command
def manager():
    q = multiprocessing.JoinableQueue()
    ev = multiprocessing.Event()

    def done(*args, **kwargs):
        print("?", args, kwargs)
        q.task_done()

    def fail(*args, **kwargs):
        print("?!", args, kwargs)
        q.task_done()

    q.put("hello")
    q.put("hello")
    # q.put(None)
    with multiprocessing.Pool() as pool:
        while not ev.is_set():
            try:
                try:
                    item = q.get(timeout=0.5)
                    pool.apply_async(
                        partial(_worker, uid=0, item=item),
                        callback=done,
                        error_callback=fail,
                    )
                except queue.Empty:
                    time.sleep(0.5)
                    continue
                except multiprocessing.TimeoutError:
                    continue
            except KeyboardInterrupt:
                ev.set()

        ev.wait()
        q.join()
