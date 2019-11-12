@decorator("after")
def foo(x, y, z) -> int:
    """after"""
    pass


def boo(x) -> int:
    """after"""
    pass


class A:
    """after"""

    def f(self, x):  # oyoyo
        """after"""
        pass

    def g(self, *args, **kwargs):  # oyoyo
        """after"""
        pass
