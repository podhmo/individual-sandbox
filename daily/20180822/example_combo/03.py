class Model:
    def __init__(self, data):
        self.data = data
        self.commit = {
            "first": None,
            "second": None,
        }

    def first(self):
        return list(self.data.keys())

    def second(self):
        return self.data.get(self.commit["first"]) or []


class TopController:
    def __init__(self, model, make_view):
        self.model = model

        def output(value):
            self.model.commit["first"] = value

        self.view = make_view(input=self.model.first, output=output)


class SubController:
    def __init__(self, model, make_view):
        self.model = model

        def output(value):
            self.model.commit["second"] = value

        self.view = make_view(input=self.model.second, output=output)


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
                self.output(self.value)


def main():
    from functools import partial
    model = Model(
        {
            "N": ["0", "1", "2", "3", "4", "5"],
            "J": ["あ", "い", "う", "え", "お"],
            "E": ["a", "b", "c", "d", "e"],
        }
    )
    top = TopController(model, partial(Select, "top"))
    sub = SubController(model, partial(Select, "sub"))

    def loop():
        while True:
            yield f"{model.commit} -- \t<{top.view}> -- <{sub.view}>"

    loop = iter(loop())
    print(next(loop))
    model.commit["first"] = "E"  # from data
    print(next(loop))
    top.view.select("J")  # from presentation
    print(next(loop))
    sub.view.select("い")
    print(next(loop))
    top.view.select("E")
    print(next(loop))
    print(next(loop))


main()
