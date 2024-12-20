from yieldfixture import create, with_context
run, yield_fixture = create()


@yield_fixture
@with_context
def f(ctx):
    i = ctx["i"] = ctx.get("i", -1) + 1
    print("{}>>> f".format(" " * i))
    try:
        yield 1
    finally:
        print("{}>>> f".format(" " * i))


@yield_fixture
@with_context
def g(ctx):
    i = ctx["i"] = ctx.get("i", -1) + 1
    print("{}>>> g".format(" " * i))
    try:
        yield 2
    finally:
        print("{}>>> g".format(" " * i))


@run
def use_it(x, y, *, i=0):
    print("{}{} + {} = {}".format(" " * (i + 1), x, y, x + y))


@run([g, f])
def use_it2(x, y, *, i=0):
    print("{}{} + {} = {}".format(" " * (i + 1), x, y, x + y))
