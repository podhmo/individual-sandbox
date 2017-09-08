class A:
    pass


class B:
    def __len__(self):
        return 0


class C:
    def __bool__(self):
        return False

if A():
    print("A true")
else:
    print("A false")

if B():
    print("B true")
else:
    print("B false")

if C():
    print("C true")
else:
    print("C false")

