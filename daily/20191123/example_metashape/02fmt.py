import string


class Formatter(string.Formatter):
    def __init__(self):
        self.d = {}

    def get_value(self, key: str, args: list, kwargs: dict) -> object:
        self.d[key] = key
        return self.d[key]


fmt = "/people/{person_id}/{x}"
f = Formatter()
print(f.format(fmt))
print(f.d)
f.d.clear()
