def f():
    try:
        g()
    except:
        raise Exception("oops")


def g():
    raise Exception("hmm")

f()
