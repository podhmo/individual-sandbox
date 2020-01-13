class A:
    def __eq__(self, x):
        return "foo"


print(A() == A())
print(A() != A())
