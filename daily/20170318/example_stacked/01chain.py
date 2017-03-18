class Chain(object):
    def __init__(self, prototype):
        self.prototype = prototype

    def __getattr__(self, name):
        return getattr(self.prototype, name)


class Top(object):
    def __init__(self, top):
        self.top = top


class Middle(Chain):
    def __init__(self, middle, top):
        super().__init__(top)
        self.middle = middle


class Bottom(Chain):
    def __init__(self, bottom, middle):
        super().__init__(middle)
        self.bottom = bottom

stacked = Top("hai")
stacked = Middle("hoi", stacked)
stacked = Bottom("yay", stacked)
print(stacked.top)
print(stacked.middle)
print(stacked.bottom)
