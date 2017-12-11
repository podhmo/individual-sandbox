from concurrent.futures import ThreadPoolExecutor
from collections import defaultdict
import threading


class Cell:
    def __init__(self):
        self._ev = threading.Event()
        self.doc = None

    def set(self, url, doc):
        self.doc = doc
        self._ev.set()

    def get(self, url):
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
        self.docs = defaultdict(Cell)

    def set(self, url, doc):
        print("B set", url, doc)
        self.docs[url].set(url, doc)
        print("A set", url, doc)

    def get(self, url):
        print("B get", url)
        cell = self.docs[url]
        v = cell.get(url)
        print("A get", url)
        return v


def run(cache, i):
    url = "http://foo.bar.jp/api"
    doc = cache.get(url)
    if doc is None:
        doc = f"*new*{i}"
        cache.set(url, doc)
    print("use", i, doc)


cache = InMemoryCache()
with ThreadPoolExecutor() as ex:
    for i in range(3):
        j = i
        ex.submit(run, cache, j)
print("*done*")
