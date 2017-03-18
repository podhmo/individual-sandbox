import abc
import unittest
import random
import contextlib
from collections import deque


class Stacked(object):
    def __init__(self):
        self.stack = deque()

    def push(self, frame):
        self.stack.append(frame)

    def pop(self):
        self.stack.pop()

    def __getattr__(self, name):
        for d in reversed(self.stack):
            if name in d:
                return d[name]
        raise AttributeError(name)

    @contextlib.contextmanager
    def scope(self, frame):
        self.push(frame)
        try:
            yield frame
        finally:
            self.pop()


class Generator(object):
    def generate_row(self, top, bottom):
        return {"data": bottom}

    def generate_data(self, top, middle, rows):
        return {"middle": middle, "rows": rows}


class Fetcher(object):
    def __init__(self, i, j, k):
        self.i = i
        self.j = j
        self.k = k
        print("i, j, k = ", i, j, k)

    def fetch_top(self):
        return ["top{}".format(i)for i in range(self.i)]

    def fetch_middle(self, top):
        return ["middle{}".format(i)for i in range(self.j)]

    def fetch_bottom(self, top, middle):
        return ["bottom{}".format(i)for i in range(self.k)]


class AbstractFilter(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def filter_top(self, tops):
        pass

    @abc.abstractmethod
    def filter_middle(self, middles):
        pass

    @abc.abstractmethod
    def filter_bottom(self, bottoms):
        pass


class DummyFilter(AbstractFilter):
    def filter_top(self, tops):
        return iter(tops)

    def filter_middle(self, middles):
        return iter(middles)

    def filter_bottom(self, bottoms):
        return iter(bottoms)


class Filter(AbstractFilter):
    def __init__(self, stacked):
        self.stacked = stacked

    def filter_top(self, tops):
        for top in tops:
            if top.endswith(("0", "2", "4", "6", "8")):
                print("drop by top:", top)
                continue
            with self.stacked.scope({"top": top}):
                yield top

    def filter_middle(self, middles):
        for middle in middles:
            # changeable
            if middle.endswith(("0", "2", "4", "6", "8")):
                print("drop by middle:", middle)
                continue
            with self.stacked.scope({"middle": middle}):
                yield middle

    def filter_bottom(self, bottoms):
        for bottom in bottoms:
            # changeable
            middle = self.stacked.middle
            top = self.stacked.top
            if bottom.endswith(("1", "3", "5", "7", "9")) and middle.endswith("1") and top.endswith("1"):
                print("drop by bottom:", bottom, middle, top)
                continue
            with self.stacked.scope({"bottom": bottom}):
                yield bottom


class Filter2(object):
    def filter_rows(self, rows):
        sorted_rows = sorted(rows, key=lambda x: random.random())
        if sorted_rows[0]["data"].endswith("0"):
            print("drop by generated rows")
        else:
            yield sorted_rows

    def filter_data(self, data):
        if random.random() > 0.5:
            print("drop by generated data")
        else:
            yield data


class Runner(object):
    def __init__(self, fetcher):
        self.fetcher = fetcher

    def run(self, filter=None, filter2=None):
        result = []
        stacked = Stacked()
        filter = filter or Filter(stacked)
        filter2 = filter2 or Filter2()
        generator = Generator()

        for top in filter.filter_top(self.fetcher.fetch_top()):
            for middle in filter.filter_middle(self.fetcher.fetch_middle(top)):
                rows = []
                for bottom in filter.filter_bottom(self.fetcher.fetch_bottom(top, middle)):
                    rows.append(generator.generate_row(top, bottom))

                for rows in filter2.filter_rows(rows):
                    for data in filter2.filter_data(generator.generate_data(top, middle, rows)):
                        result.append(data)
        print(len(result))
        return result


class TestRunner(unittest.TestCase):
    def test_it(self):
        runner = Runner(Fetcher(4, 4, 4))
        result = runner.run(filter=DummyFilter())
        print(result)


if __name__ == "__main__":
    # Runner(Fetcher(i=100, j=100, k=100)).run()
    unittest.main()
