def sorted_dedup(xs):
    return sorted(set(xs), key=xs.index)


L = [2, 2, 1, 3, 4, 5, 4]

print(L)
print(sorted_dedup(L))
