class Foo(object):
    def __init__(self, name):
        self.name = name
        self.desc = None
        self.saved = False

    def save(self):
        self.saved = True
