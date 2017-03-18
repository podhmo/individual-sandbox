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


class Runner(object):
    def __init__(self):
        self.fetcher = Fetcher(i=100, j=100, k=100)

    def run(self):
        generator = Generator()
        result = []
        for top in self.fetcher.fetch_top():
            for middle in self.fetcher.fetch_middle(top):
                rows = []
                for bottom in self.fetcher.fetch_bottom(top, middle):
                    rows.append(generator.generate_row(top, bottom))
                result.append(generator.generate_data(top, middle, rows))
        print(len(result))
        return result


if __name__ == "__main__":
    Runner().run()
