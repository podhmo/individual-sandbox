class PaginatedResponse:
    def __init__(self, response):
        self.response_list = [response]
        self.i = 0

    @property
    def response(self):
        return self.response_list[self.i]

    def append(self, response):
        self.response_list.append(response)

    def __getattr__(self, name):
        return getattr(self.response, name)

    def __next__(self):
        self.seek(self.i + 1)
        if self.i >= len(self.response_list):
            raise StopIteration(self.i)
        return self

    def seek(self, i):
        self.i = i

    def __iter__(self):
        return self


class DummyResponse:
    def __init__(self, status_code, data):
        self.status_code = status_code
        self.data = data

    def json(self):
        return self.data

paginated = PaginatedResponse(DummyResponse(200, list(range(10))))
paginated.append(DummyResponse(200, list(range(3))))

print("first response")
print(paginated.status_code)
print(paginated.json())

for response in paginated:
    print(response.json())
