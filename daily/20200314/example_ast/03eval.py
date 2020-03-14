from q import Q, QEvaluator


def q(val, builder=QEvaluator(), **kwargs):
    return Q(builder, val, kwargs)


print(q(10).val, q(10).add(20).add(30))
print(q("foo").upper())
