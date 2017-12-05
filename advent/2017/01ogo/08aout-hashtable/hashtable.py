BUCKET_SIZE = 8
SIZE = 16


def make_bucket():
    return [[None, None] for _ in range(BUCKET_SIZE)]


def make():
    return [make_bucket() for _ in range(SIZE)]


def assign(m, k, v):
    b = m[k % SIZE]
    for pair in b:
        if pair[0] is None:
            pair[0] = k
            pair[1] = v
            break


def get(m, k):
    b = m[k % SIZE]
    for pair in b:
        if pair[0] == k:
            return pair[1]
    return None


def iterate(m):
    for b in m:
        for x in b:
            if x[0] is None:
                break
            yield x
