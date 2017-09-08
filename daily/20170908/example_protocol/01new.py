class A:
    def __new__(self):
        return B()


class B:
    pass


class C:
    def __new__(self):
        return 1

print(A())
print(C())
