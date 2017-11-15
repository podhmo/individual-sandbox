import random
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from collections import defaultdict, deque
from threading import Semaphore
from queue import Queue


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))
    return i


def with_limitation(sem):
    def do(fn, *args, **kwargs):
        with sem:
            return fn(*args, **kwargs)

    return do


class RoundrobinQueue(Queue):
    '''Variant of Queue'''

    def _get_key(self, item):
        return item

    def _init(self, maxsize):
        self.store = defaultdict(deque)
        self.c = 0
        self.key_iterator = None

    def _qsize(self):
        # # not good performance
        # return sum(len(v) for v in self.store.values())
        return self.c

    def _put(self, item):
        self.store[self._get_key(item)].append(item)
        self.c += 1

    def _get(self, retry=False):
        if self.key_iterator is None:
            self.key_iterator = iter(self.store.keys())
        try:
            k = next(self.key_iterator)
            dq = self.store.get(k)
            if dq is None:
                return self._get(retry=True)
            elif not dq:
                return self._get(retry=True)
            else:
                self.c -= 1
                return dq.popleft()
        except StopIteration:
            if retry:
                raise
            self.key_iterator = None
            return self._get(retry=True)


q = RoundrobinQueue()
xs = (["x"] * 3) + (["y"] * 5) + (["z"] * 4) + (["i"] * 5) + (["j"] * 3)
sem_map = defaultdict(lambda: Semaphore(2))

print("S")
# enqueue
for x in xs:
    q.put(x)

with ThreadPoolExecutor() as ex:
    futs = []
    while not q.empty():
        x = q.get()
        futs.append(ex.submit(with_limitation(sem_map[x]), do_task, x))
    for f in as_completed(futs):
        print("ok", f.result())
print("E")
