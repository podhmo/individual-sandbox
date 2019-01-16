class X:
    def f(self, x):
        return self.g(x, 1)

    def g(self, x, ans):
        if x == 0:
            return ans
        else:
            return self.g(x - 1, x * ans)


def f(x):
    return g(x, 1)


def g(x, ans):
    if x == 0:
        return ans
    else:
        return g(x - 1, x * ans)
