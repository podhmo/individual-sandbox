class Foo(object):
    def __init__(self, **kwargs):
        if "name" in kwargs:
            self.name = kwargs["name"]
        else:
            self.name = None
        if "desc" in kwargs:
            self.desc = kwargs["desc"]
        else:
            self.desc = None
        self.saved = False

    def save(self):
        print("saved. name={0}, desc={1}".format(self.name, self.desc))
        self.saved = True
