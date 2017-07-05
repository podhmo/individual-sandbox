def filter(xs, new_item, shape):
    nl = []
    for x in xs:
        ok = True
        for i in range(len(x)):
            if shape[i]:  # bigger
                if new_item[i] <= x[i]:
                    ok = False
            else:
                if new_item[i] >= x[i]:
                    ok = False

        if ok:
            nl.append(x)
    return nl
