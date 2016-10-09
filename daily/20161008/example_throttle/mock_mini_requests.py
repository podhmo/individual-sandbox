from mock_request import MockRequest as R
requests = [
    R("x", 1).add_link(R("xy", 1)).add_link(R("xy", 2)).add_link(R("xy", 3)),
    R("y", 1),
    R("y", 2),
    R("y", 11).add_link(R("yz", 1)).add_link(R("yz", 2)).add_link(R("yz", 3)),
    R("y", 12),
    R("x", 2),
    R("y", 3),
    R("z", 1),
    R("x", 3),
]

