def flexible_decorator(func=None, *, initial_msg="started", final_msg="ended"):
    def wrap(f):
        def do(*args, **kwargs):
            print(initial_msg)
            retval = f(*args, **kwargs)
            print(final_msg)
            return retval

        return do

    if func is None:
        return wrap
    return wrap(func)


@flexible_decorator
def hello(x: str):
    print("hello", x)


@flexible_decorator(initial_msg="S", final_msg="E")
def bye(x: str):
    print("bye", x)


def main():
    print(hello("world"), "*HELLO*")
    print(bye("world"), "*BYE*")


main()
