from yieldfixture import create
run, yield_fixture = create()


@yield_fixture
def f():
    print(">>> f")
    yield 1
    print(">>> f")


@yield_fixture
def g():
    print("  >>> g")
    yield 2
    print("  >>> g")


@run
def use_it(x, y):
    print("{} + {} = {}".format(x, y, x + y))
