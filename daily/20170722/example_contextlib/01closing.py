import contextlib


@contextlib.contextmanager
def closing(response):
    try:
        yield response
    finally:
        response.close()


class Response:
    def __init__(self):
        print("open")

    def close(self):
        print("close")


with closing(Response()) as response:
    print(response)

with contextlib.closing(Response()) as response:
    print(response)
