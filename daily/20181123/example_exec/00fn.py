def f():
    pass


class C:
    def f(self):
        pass


print("@", f.__module__, "@")
print("@", C.f.__module__, "@")
