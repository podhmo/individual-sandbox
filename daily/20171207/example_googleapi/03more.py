from concurrent.futures import ThreadPoolExecutor
from collections import defaultdict
import threading


class _Cell:
    def __init__(self):
        self._ev = threading.Event()
        self.doc = None

    def set(self, doc):
        self.doc = doc
        self._ev.set()

    def get(self):
        if self._ev.is_set():
            self._ev.wait()
        else:
            if self.doc is not None:
                self._ev.set()
            else:
                self._ev.clear()
        return self.doc


class InMemoryCache:
    def __init__(self):
        self.docs = defaultdict(_Cell)

    def set(self, url, doc):
        self.docs[url].set(doc)
        print("@set", url, doc)

    def get(self, url):
        cell = self.docs[url]
        v = cell.get()
        return v


def run(cache, i):
    url = "http://foo.bar.jp/api"
    doc = cache.get(url)
    if doc is None:
        doc = f"*new*{i}"
        cache.set(url, doc)
    print("use", i, doc)


def run2(cache, i):
    url = "http://foo.boo.jp/api"
    doc = cache.get(url)
    if doc is None:
        doc = f"*new*{i}"
        cache.set(url, doc)
    print("use", i, doc)


cache = InMemoryCache()
with ThreadPoolExecutor() as ex:
    for i in range(4):
        j = i
        ex.submit(run, cache, j)
        ex.submit(run2, cache, j)
