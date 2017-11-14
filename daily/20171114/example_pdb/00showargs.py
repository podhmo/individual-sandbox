def f(x, y, z):
    print(locals())
    return (x, y, z)

f(10, 20, 30)
# {'z': 30, 'y': 20, 'x': 10}
