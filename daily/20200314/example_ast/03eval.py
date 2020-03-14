from q import Q, QEvaluator


def q(val, builder=QEvaluator(), **kwargs):
    return Q(builder, val, kwargs)


print(q(10).val, q(10).Add(20).Add(30))
print(q("foo").upper())
