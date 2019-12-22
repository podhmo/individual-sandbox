import contextlib
from handofcats import as_command


@contextlib.contextmanager
def handle():
    print("start")
    try:
        yield
    except Exception as e:
        print(e)
        import traceback

        print(traceback.format_exc())
    print("end")


@as_command
def hello():
    with handle():
        1 / 0
        print("never")
