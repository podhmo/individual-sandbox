L = [1, 2, 3]
idx = 0
for i, x in enumerate(L):
    if x == 2:
        idx = i
L.insert(i, 10)
print(L)
