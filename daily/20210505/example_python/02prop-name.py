class X:
    def __set_name__(self, owner, name):
        print(name, self, owner)
        self.name = name


class A:
    x = X()
