from mock_request import MockRequest
R = MockRequest

requests = [
    R("http://sample.y.net/", 1),
    R("http://sample.y.net/", 2),
    R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)).add_link(R("http://sample.y.net/z", 3)),
    R("http://example.x.com/", 1).add_link(R("http://example.x.com/y", 1)).add_link(R("http://example.x.com/y", 2)).add_link(R("http://example.x.com/y", 3)),
    R("http://sample.y.net/", 12),
    R("http://example.x.com/", 2),
    R("http://sample.y.net/", 3),
    R("http://otameshi.z.jp/", 1),
    R("http://example.x.com/", 3),
    R("http://example.x.com/", 1).add_link(R("http://example.x.com/y", 1)).add_link(R("http://example.x.com/y", 2)).add_link(R("http://example.x.com/y", 3)),
    R("http://sample.y.net/", 1),
    R("http://sample.y.net/", 2),
    R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)).add_link(R("http://sample.y.net/z", 3)),
    R("http://sample.y.net/", 12),
    R("http://example.x.com/", 2),
    R("http://sample.y.net/", 3),
    R("http://otameshi.z.jp/", 1),
    R("http://example.x.com/", 3),
    R("http://example.x.com/", 1).add_link(R("http://example.x.com/y", 1)).add_link(R("http://example.x.com/y", 2)).add_link(R("http://example.x.com/y", 3)),
    R("http://sample.y.net/", 1),
    R("http://sample.y.net/", 2),
    R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)).add_link(R("http://sample.y.net/z", 3)),
    R("http://sample.y.net/", 12),
    R("http://example.x.com/", 2),
    R("http://sample.y.net/", 3),
    R("http://otameshi.z.jp/", 1),
    R("http://example.x.com/", 3),
    R("http://example.x.com/", 1).add_link(R("http://example.x.com/y", 1)).add_link(R("http://example.x.com/y", 2)).add_link(R("http://example.x.com/y", 3)),
    R("http://sample.y.net/", 1),
    R("http://sample.y.net/", 2),
    R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)).add_link(R("http://sample.y.net/z", 3)),
    R("http://sample.y.net/", 12),
    R("http://example.x.com/", 2),
    R("http://sample.y.net/", 3),
    R("http://otameshi.z.jp/", 1),
    R("http://example.x.com/", 3),
]


def size(request):
    return 1 + sum(map(size, request.get_links()))
print(sum(map(size, requests)))
