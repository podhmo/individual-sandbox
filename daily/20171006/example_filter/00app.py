from nbreversible import code


class App:
    def __init__(self, *, collector=None):
        self.collector = collector or ActualCollector()

    def do_items(self, items):
        yield from self.collector.collect_items(items)

    def do_calculate(self, items):
        result = [i * i for i in items]
        yield from self.collector.collect_result(result)

    def run(self):
        items = list(range(100))
        for items in self.do_items(items):
            for result in self.do_calculate(items):
                yield result


class NoopCollector:
    def collect_items(self, items):
        yield items

    def collect_result(self, result):
        yield result


class ActualCollector:
    def collect_items(self, items):
        yield items[:10]

    def collect_result(self, result):
        r = []
        for i in result:
            if i % 2 == 0:
                print("drop", i)
                continue
            r.append(i)
        yield r

with code():
    result = list(App().run())
    print("@", result)
    # result = list(App(collector=NoopCollector()).run())
    # print("@@", result)
