def _all_keys(xs, ys):
    if not xs:
        return ys
    if not ys:
        return xs

    seen = set(xs)

    r = []
    buf = []
    i, j, N, M = 0, 0, len(xs), len(ys)
    for j in range(M):
        y = ys[j]
        if not seen:
            buf.append(y)
            continue

        while y != xs[i]:
            r.append(xs[i])
            i += 1
            if i >= N:
                break

        buf.append(y)
        r.extend(buf)
        buf = []

        i += 1
        if i >= N:
            j += 1
            break

    for k in range(j, M):
        r.append(ys[k])

    if buf:
        r.extend(buf)

    while i < N:
        r.append(xs[i])
        i += 1
    return r


print(_all_keys([1, 2, 3], [1, 2, 3, 4]))
print(_all_keys([0, 1, 2, 3], [1, 2, 3, 4]))
print(_all_keys([1, 0, 2, 3], [1, 2, 3, 4]))
print(_all_keys([1, 0, -1, 2, 3], [1, 2, 3, 4]))
print(_all_keys([0, 1, 2, 3, 5], [1, 2, 3, 4]))
