class Select:
    def __init__(self, name, *, input, output, value=None):
        self.name = name
        self.input = input
        self.output = output
        self.value = value

    def __str__(self):
        candidates = self.input()
        if self.value not in candidates:
            if len(candidates) > 0:
                self.select(candidates[0])
        return f"{self.name}: {self.value} (in {candidates})"

    def select(self, value):
        for v in self.input():
            if v == value:
                self.value = value
                self.output(self.name, self.value)


class Handler:
    def __init__(self, store, *, actions=None):
        self.store = store
        self.actions = actions or {}

    def register(self, fn, *, name=None):
        self.actions[name or fn.__name__] = fn
        return fn

    def __call__(self, name, value):
        action = self.actions.get(name)
        if action is not None:
            action(self.store, value)


def main():
    def make0(store, *, name):
        def set0(store, value):
            store["commit"][0] = value
            store["subdata"] = store["data"][value]

        return Select(name, output=h, input=lambda: list(store["data"].keys())), set0

    def make1(store, *, name):
        def set1(store, value):
            store["commit"][1] = value

        return Select(name, output=h, input=lambda: store["data"].get(store["commit"][0])), set1

    store = {
        "commit": [None, None],
        "data": {
            "N": ["0", "1", "2", "3", "4", "5"],
            "J": ["あ", "い", "う", "え", "お"],
            "E": ["a", "b", "c", "d", "e"],
        }
    }

    h = Handler(store)
    top, action = make0(store, name="0")
    h.register(action, name=top.name)
    sub, action = make1(store, name="1")
    h.register(action, name=sub.name)

    def loop():
        while True:
            params = yield f"{store['commit']} -- \t<{top}> -- <{sub}>"
            if params is not None:
                h(*params)

    loop = iter(loop())
    print(next(loop))
    loop.send(("0", "E"))  # from data
    print(next(loop))
    top.select("J")  # from presentation
    print(next(loop))
    sub.select("い")
    print(next(loop))
    top.select("E")
    print(next(loop))
    print(next(loop))


main()
