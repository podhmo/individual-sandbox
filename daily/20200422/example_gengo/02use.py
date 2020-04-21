def get_walker():
    from walk import Walker

    return Walker()


def hello():
    w = get_walker()
    w.walk("hello")
