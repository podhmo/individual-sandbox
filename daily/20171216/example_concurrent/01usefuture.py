import unittest
from concurrent.futures import Future
from collections import defaultdict


class InMemoryCache:
    def __init__(self, *, timeout=1, future_factory=Future):
        self.futs = {}
        self.future_factory = future_factory
        self.timeout = timeout

    def set(self, url, doc):
        fut = self.futs.get(url) or self.future_factory()
        fut.set_result(doc)

    def get(self, url):
        fut = self.futs.get(url)
        if fut is None:
            self.futs[url] = self.future_factory()
            return None
        else:
            return fut.result(timeout=self.timeout)


class ConcurrentTests(unittest.TestCase):
    def _makeOne(self):
        class _WithCount(InMemoryCache):
            c = defaultdict(int)

            def set(self, url, doc):
                self.c[url] += 1
                return super().set(url, doc)

        return _WithCount()

    def test_it(self):
        from concurrent.futures import ThreadPoolExecutor, as_completed

        cache = self._makeOne()

        k = "*key*"

        def access(i):
            v = cache.get(k)
            if v is not None:
                return v
            cache.set(k, i)
            return i

        with ThreadPoolExecutor(10) as ex:
            futs = []
            for i in range(10):
                futs.append(ex.submit(access, i))

            for fut in as_completed(futs):
                self.assertEqual(fut.result(), 0, msg="cached value is returned")

        self.assertEqual(cache.c[k], 1, msg="set() is called only once")


if __name__ == "__main__":
    unittest.main()
