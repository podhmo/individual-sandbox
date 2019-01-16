class A:
    pass


class B(A):
    pass


def use(a: A) -> None:
    pass

use(B())
