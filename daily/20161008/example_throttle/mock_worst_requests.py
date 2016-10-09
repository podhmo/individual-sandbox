from mock_request import MockRequest as R

requests = [
    R("http://sample.y.net/", 1),
    R("http://sample.y.net/", 2),
    R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)),
    R("http://sample.y.net/", 1),
    R("http://sample.y.net/", 2),
    R("http://sample.y.net/", 1),
    R("http://sample.y.net/", 2),
    R("http://sample.a.net/", 1),
    R("http://sample.b.net/", 1),
    R("http://sample.c.net/", 1),
    R("http://sample.d.net/", 1),
    R("http://sample.e.net/", 1),
    R("http://sample.f.net/", 1),
    R("http://sample.g.net/", 1),
    R("http://sample.h.net/", 1),
    R("http://sample.i.net/", 1),
    R("http://sample.y.net/", 1),
    R("http://sample.y.net/", 2),
]

