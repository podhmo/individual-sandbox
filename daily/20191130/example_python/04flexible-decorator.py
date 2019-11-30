from functools import partial, update_wrapper, wraps


# TODO: update wrapper
# TODO: call internal function


def flexible_decorator(func=None, *, initial_msg="started", final_msg="ended"):
    def do(f, *args, **kwargs):
        print(initial_msg)
        retval = f(*args, **kwargs)
        print(final_msg)
        return retval

    if func is None:
        return lambda f: update_wrapper(partial(do, f), f)
    return update_wrapper(partial(do, func), func)


@flexible_decorator
def hello(x: str):
    "hello hello"
    print("hello", x)


@flexible_decorator(initial_msg="S", final_msg="E")
def bye(x: str):
    "bye bye"
    print("bye", x)


def tiny_decorator(f):
    @wraps(f)
    def do(*args, **kwargs):
        print("ts")
        retval = f(*args, **kwargs)
        print("te")
        return retval

    return do


@tiny_decorator
def boo(x: str):
    "bye bye"
    print("bye", x)


def main():
    print(hello("world"), "*HELLO*")
    print(bye("world"), "*BYE*")

    print(hello.args[0]("WORLD"))
    import inspect

    print("**", inspect.getdoc(hello))

    import pydoc
    d = pydoc.TextDoc()
    print(d.docroutine(boo))
    print(d.docroutine(hello))

    class Doc(pydoc.TextDoc):
        pass
    print(inspect.isroutine(hello), inspect.signature(hello))
main()
