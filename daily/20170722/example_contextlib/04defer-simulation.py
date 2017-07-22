import contextlib


class Response:
    def __init__(self, name):
        self.name = name
        print("open", self.name)

    def close(self):
        print("close", self.name)


with contextlib.ExitStack() as s:
    response = Response("1")
    s.callback(response.close)
    response = Response("2")
    s.callback(response.close)

    print("use", response.name, response)
    1 / 0
    print("use", response.name, response)
