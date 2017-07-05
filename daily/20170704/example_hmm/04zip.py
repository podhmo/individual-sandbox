import operator as op


def detect_predicates(shape):
    return [op.gt if x else op.lt for x in shape]


def filter(items, new_item, shape):
    nl = []
    ps = detect_predicates(shape)
    for item in items:
        ok = True
        for x, n, p in zip(item, new_item, ps):
            if not p(n, x):
                ok = False
                break
        if ok:
            nl.append(item)
    return nl
