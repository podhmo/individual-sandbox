import pydoc


class A:
    def a(self, x):
        pass

    def m(self, x):
        pass

    @classmethod
    def c(cls, x):
        pass


class B(A):
    def b(self, x):
        pass

    def m(self, x):
        pass


print(pydoc.plaintext.document(B))
print(B.__dict__)
