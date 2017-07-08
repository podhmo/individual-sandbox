from pympler.asizeof import asizeof

n = 1
print("int", asizeof(n))

L = []
print("list(len={})".format(len(L)), asizeof(L))

L = [1]
print("list(len={})".format(len(L)), asizeof(L))

L = [1, 2]
print("list(len={})".format(len(L)), asizeof(L))

L = [1, 2, 3]
print("list(len={})".format(len(L)), asizeof(L))
