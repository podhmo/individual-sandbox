import sys


def foo():
    print("foo")
    bar()
    print("foo")


def bar():
    print("bar")
    boo()
    print("bar")


def boo():
    print("boo")
    raise Exception("oops")


# https://www.oreilly.com/library/view/python-cookbook/0596001673/ch14s06.html
def info(type, value, tb):
    if hasattr(sys, "ps1") or not sys.stderr.isatty():
        # You are in interactive mode or don't have a tty-like
        # device, so call the default hook
        sys.__excepthook__(type, value, tb)
    else:
        import traceback, pdb

        # You are NOT in interactive mode; print the exception...
        traceback.print_exception(type, value, tb)
        # ...then start the debugger in post-mortem mode
        pdb.post_mortem(tb)


sys.excepthook = info
foo()
