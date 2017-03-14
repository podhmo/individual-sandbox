import random

S = [set(), {"A"}, {"B"}, {"C"}, {"A", "B"}, {"A", "C"}, {"B", "C"}, {"A", "B", "C"}]
random.seed(2)
random.shuffle(S)


def lower(x, ys):
    return [y for y in ys if y.issubset(x)]


def upper(x, ys):
    return [y for y in ys if y.issuperset(x)]


# lower
# {A, B} -> {A, B}, {A}, {B}, {}
# {A, D} -> {A}, {}
print(lower({"A", "B"}, S))
print(lower({"A", "D"}, S))


# upper
# {A, B} -> {A, B}, {A, B, C}
# {A, D} ->
print(upper({"A", "B"}, S))
print(upper({"A", "D"}, S))
