class Properties:
    @property
    def value(self):
        return get_value()


# class Cached:
#     @property # error
#     def value(self):
#         self.value = get_value()
#         return self.value


class cached:
    def __init__(self, fn):
        self.fn = fn

    # data descriptor
    def __get__(self, ob, objtype=None):
        if ob is None:
            return self
        val = self.fn(ob)
        setattr(ob, self.fn.__name__, val)
        return val


class Cached:
    @cached
    def value(self):
        return get_value()


def get_value():
    print("called")
    return 42


p = Properties()
print(p.value)
print(p.value)
print("----------------------------------------")
p = Cached()
print(p.value)
print(p.value)
