class A:
    def __init__(self, value):
        self.value = value

    @property
    def value(self):
        return "@value"

    @value.setter
    def value(self, x):
        print("set", x)


a = A("*value")
print(a.value)
