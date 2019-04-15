class Base:
    def __init__(self, status, v):
        self.status = status
        self.v = v


class A(Base):
    def __init__(self, v):
        super().__init__("A", v)


print(A("hello").status)
