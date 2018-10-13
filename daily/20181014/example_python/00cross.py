def cross(*xss):
    if not xss:
        return []
    if len(xss) == 1:
        return [(x, ) for x in xss[0]]
    return [(x, *vs) for x in xss[0] for vs in cross(*xss[1:])]


print(cross([1]))
print(cross([1, 2], ["a", "b"]))
print(cross([1, 2], ["a", "b"], ["i", "j"]))
