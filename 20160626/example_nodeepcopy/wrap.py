import copy


class A(object):
    pass


a = A()
print(a)
print(copy.deepcopy(a))
assert id(copy.deepcopy(a)) != id(a)


class B(object):
    def __deepcopy__(self, m):
        return self


b = B()
print(b)
print(copy.deepcopy(b))
assert id(copy.deepcopy(b)) == id(b)
