

response = PaginatedResponse(FixedPaginator([DummyResponse(200, [1, 2, 3]), DummyResponse(200, [4, 5, 6]), DummyResponse(200, [7, 8])]))
print(response.response)
for value in response:
    print(value.json())
print("owari")
