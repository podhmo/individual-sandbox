class Base2:
    status: str = "base"

    def __init__(self, v):
        self.v = v


class A2(Base2):
    status = "A"



print(A2("hello").status)
