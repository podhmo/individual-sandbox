def show(xs, ys):
    n = min(len(xs), len(ys))
    for i in range(n):
        print(i, "@", xs[i], ys[i])

    if n == len(xs):
        for j in range(n, len(ys)):
            print(j, None, ys[j])
    else:
        for j in range(n, len(xs)):
            print(j, xs[j], None)


xs = [1, 2, 3]
ys = [1, 2]
show(xs, ys)
print("----------------------------------------")
show(ys, xs)
