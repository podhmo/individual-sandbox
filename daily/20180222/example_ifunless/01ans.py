class X:
    def __init__(self, status):
        self.status = status

    def __bool__(self):
        self.status = not self.status
        return self.status


def f(x):
    if x:
        if not x:
            print("hai")


if __name__ == "__main__":
    f(X(False))
