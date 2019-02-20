class A:
    def __enter__(self):
        print("enter")
        return self

    def __exit__(self, a, b, c):
        print("exit", a, b, c)


a = A()
with a:
    with a:
        print("hai")
