def filter(xs, new_item, shape):
    nl = []
    for x in xs:
        remove = True
        for i in range(len(x)):
            if shape[i]:  # bigger
                if new_item[i] > x[i]:
                    pass
                else:
                    remove = False
            else:
                if new_item[i] < x[i]:
                    pass
                else:
                    remove = False
        if not remove:
            nl.append(x)
    return nl
