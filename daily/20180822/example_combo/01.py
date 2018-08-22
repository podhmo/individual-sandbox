# function as stream


class Select:
    def __init__(self, name, candidates, value=None):
        self.name = name
        self.candidates = candidates
        self.value = value

    def __str__(self):
        candidates = self.candidates()
        if self.value not in candidates:
            if len(candidates) > 0:
                self.select(candidates[0])
        return f"{self.name}: {self.value} (in {candidates})"

    def select(self, value):
        for v in self.candidates():
            if v == value:
                self.value = value
                CHANGE(self.name, self.value)


store = {
    "commit": [None, None],
    "data": {
        "N": ["0", "1", "2", "3", "4", "5"],
        "J": ["あ", "い", "う", "え", "お"],
        "E": ["a", "b", "c", "d", "e"],
    }
}


def CHANGE(name, value):
    global store
    print("----", name, value)
    if name == "top":
        set_0(store, value)
    elif name == "sub":
        set_1(store, value)


def set_0(store, value):
    store["commit"][0] = value
    store["subdata"] = store["data"][value]


def set_1(store, value):
    store["commit"][1] = value


def main():
    top = Select("top", lambda: list(store["data"].keys()))
    sub = Select("sub", lambda: store["data"].get(store["commit"][0]))

    def gen():
        while True:
            yield f"{store['commit']} -- \t<{top}> -- <{sub}>"

    loop = iter(gen())
    print(next(loop))
    top.select("J")
    print(next(loop))
    sub.select("い")
    print(next(loop))
    top.select("E")
    print(next(loop))


main()
