def f(*, x=0):
    return {"x": x}


f()  # => {'x': 0}
f(x=2)  # => {'x': 2}
f.__kwdefaults__["x"] = 10
f()  # => {'x': 10}
