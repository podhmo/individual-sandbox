import unittest
from collections import defaultdict
import threading


class _Cell:
    def __init__(self, timeout):
        self.doc = None
        self.timeout = timeout  # waiting time
        self._ev = threading.Event()
        self._marked = False
        self._set_count = 0  # for test

    def set(self, doc):
        self.doc = doc
        self._ev.set()
        self._set_count += 1

    def reset(self):
        self._marked = False
        self._ev.clear()

    def get(self):
        if self.doc is None:
            if not self._marked:
                self._marked = True
                self._ev.clear()
                return None
        self._ev.wait(self.timeout)
        return self.doc


class InMemoryCache:
    def __init__(self, timeout=5):
        self.docs = defaultdict(lambda: _Cell(timeout))

    def set(self, url, doc):
        self.docs[url].set(doc)

    def get(self, url):
        cell = self.docs[url]
        v = cell.get()
        return v


class ConcurrentTests(unittest.TestCase):
    def _makeOne(self):
        return InMemoryCache()

    def test_it(self):
        from concurrent.futures import ThreadPoolExecutor, as_completed

        cache = self._makeOne()

        k = "*key*"

        def access(i):
            try:
                v = cache.get(k)
                if v is not None:
                    return v
                cache.set(k, i)
            except Exception as e:
                print(e)
            return i

        with ThreadPoolExecutor(10) as ex:
            futs = []
            for i in range(10):
                futs.append(ex.submit(access, i))

            for fut in as_completed(futs):
                self.assertEqual(fut.result(), 0, msg="cached value is returned")

        self.assertEqual(cache.docs[k]._set_count, 1, msg="set() is called only once")


if __name__ == "__main__":
    unittest.main()
