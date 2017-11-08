def consume(xs, fn, n=0):
    if n <= 0:
        fn(xs)
    else:
        for x in xs:
            consume(x, fn, n=n - 1)


print("----------------------------------------")
xs = [["x", "y", "z"], ["a"]]
consume(xs, print)
print("----------------------------------------")
xs = [["x", "y", "z"], ["a"]]
consume(xs, print, n=1)
print("----------------------------------------")
xs = [["x", "y", "z"], ["a"]]
consume(xs, print, n=2)
