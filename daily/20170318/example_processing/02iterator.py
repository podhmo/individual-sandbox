import abc
import unittest
import random


class Generator(object):
    def generate_row(self, top, bottom):
        return {"result": bottom}

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
    def filter_middle(self, top, middles):
        pass

    @abc.abstractmethod
    def filter_bottom(self, top, middle, bottoms):
        pass


class DummyFilter(AbstractFilter):
    def filter_top(self, tops):
        return iter(tops)

    def filter_middle(self, top, middles):
        return iter(middles)

    def filter_bottom(self, top, middle, bottoms):
        return iter(bottoms)


class Filter(AbstractFilter):
    def filter_top(self, tops):
        for top in tops:
            if top.endswith(("0", "2", "4", "6", "8")):
                print("drop by top:", top)
                continue
            yield top

    def filter_middle(self, top, middles):
        for middle in middles:
            # changeable
            if middle.endswith(("0", "2", "4", "6", "8")):
                print("drop by middle:", middle)
                continue
            yield middle

    def filter_bottom(self, top, middle, bottoms):
        for bottom in bottoms:
            # changeable
            if bottom.endswith(("1", "3", "5", "7", "9")) and middle.endswith("1") and top.endswith("1"):
                print("drop by bottom:", bottom, middle, top)
                continue
            yield bottom


class Runner(object):
    def __init__(self, fetcher):
        self.fetcher = fetcher

    def run(self, filter=None):
        # testable
        filter = filter or Filter()
        generator = Generator()

        result = []
        for top in filter.filter_top(self.fetcher.fetch_top()):
            for middle in filter.filter_middle(top, self.fetcher.fetch_middle(top)):
                rows = []
                for bottom in filter.filter_bottom(top, middle, self.fetcher.fetch_bottom(top, middle)):
                    rows.append(generator.generate_row(top, bottom))
                result.append(generator.generate_data(top, middle, rows))
        print(len(result))
        return result

    def run_real(self, filter=None):
        filter = filter or Filter()
        generator = Generator()

        result = []
        for top in filter.filter_top(self.fetcher.fetch_top()):
            for middle in filter.filter_middle(top, self.fetcher.fetch_middle(top)):
                rows = []
                for bottom in filter.filter_bottom(top, middle, self.fetcher.fetch_bottom(top, middle)):
                    rows.append(generator.generate_row(top, bottom))
                # but filter by generated row.(so. not testable)
                sorted_rows = sorted(rows, key=lambda x: random.random())
                if sorted_rows[0].endswith("0"):
                    print("drop by generated rows")
                    continue
                data = generator.generate_data(top, middle, rows)
                if random.random() > 0.5:
                    print("drop by generated data")
                    continue
                result.append(data)
        print(len(result))
        return result


class DummyFilter(object):
    def filter_top(self, tops):
        return iter(tops)

    def filter_middle(self, top, middles):
        return iter(middles)

    def filter_bottom(self, top, middle, bottoms):
        return iter(bottoms)


class TestRunner(unittest.TestCase):
    def test_it(self):
        runner = Runner(Fetcher(2, 2, 2))
        result = runner.run(filter=DummyFilter())
        print(result)


if __name__ == "__main__":
    # Runner(Fetcher(i=100, j=100, k=100)).run()
    unittest.main()
