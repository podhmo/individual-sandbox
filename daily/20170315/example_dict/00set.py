from collections import namedtuple
# shape0 = {('properties', 'name', 'type', 'string'), ('type', 'object'), ('properties', 'age', 'type', 'integer')}
# shape1 = {('properties', 'name', 'type', 'string'), ('type', 'object'), ('properties', 'age', 'type', 'integer'), ('properties', 'age', 'minimum', 0)}

s0 = {"name", "age"}
s1 = {"name", "age", "alias"}
s2 = {"name", "alias"}


R = namedtuple("R", "issubset, equal, issuperset")


def f(s0, s1):
    return R(issubset=s0.issubset(s1), equal=(s0 == s1), issuperset=s0.issuperset(s1))

print(s0, s0, f(s0, s0))
print(s0, s1, f(s0, s1))
print(s0, s2, f(s0, s2))
print(s1, s1, f(s1, s1))
print(s1, s2, f(s1, s2))
print(s2, s2, f(s2, s2))
# treeset has subset?

