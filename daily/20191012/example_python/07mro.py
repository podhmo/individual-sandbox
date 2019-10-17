class A:
    pass


class B(A):
    pass


print(B.mro())
# [<class '__main__.B'>, <class '__main__.A'>, <class 'object'>]
