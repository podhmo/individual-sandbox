def filter(l, n, maximize):
    nl = []
    for x in l:
        remove = True
        for i in range(len(x)):
            if maximize[i]:
                if n[i] > x[i]:
                    pass
                else:
                    remove = False
            else:
                if n[i] < x[i]:
                    pass
                else:
                    remove = False
        if not remove:
            nl.append(x)
    return nl
