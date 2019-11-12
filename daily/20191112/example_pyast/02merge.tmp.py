[
    Leaf(1, "def"),
    Leaf(1, "foo"),
    Node(
        parameters,
        [
            Leaf(7, "("),
            Node(typedargslist, [Leaf(1, "x"), Leaf(12, ","), Leaf(1, "y")]),
            Leaf(8, ")"),
        ],
    ),
    Leaf(55, "->"),
    Leaf(1, "int"),
    Leaf(11, ":"),
    Node(
        suite,
        [
            Leaf(4, "\n"),
            Leaf(5, "    "),
            Node(simple_stmt, [Leaf(3, '"""before"""'), Leaf(4, "\n")]),
            Node(simple_stmt, [Leaf(1, "pass"), Leaf(4, "\n")]),
            Leaf(6, ""),
        ],
    ),
]
[
    Leaf(1, "def"),
    Leaf(1, "bar"),
    Node(parameters, [Leaf(7, "("), Leaf(1, "x"), Leaf(8, ")")]),
    Leaf(55, "->"),
    Leaf(1, "int"),
    Leaf(11, ":"),
    Node(
        suite,
        [
            Leaf(4, "\n"),
            Leaf(5, "    "),
            Node(simple_stmt, [Leaf(3, '"""before"""'), Leaf(4, "\n")]),
            Node(simple_stmt, [Leaf(1, "pass"), Leaf(4, "\n")]),
            Leaf(6, ""),
        ],
    ),
]
[
    Leaf(1, "def"),
    Leaf(1, "f"),
    Node(parameters, [Leaf(7, "("), Leaf(1, "self"), Leaf(8, ")")]),
    Leaf(11, ":"),
    Node(
        suite,
        [
            Leaf(4, "\n"),
            Leaf(5, "        "),
            Node(simple_stmt, [Leaf(3, '"""before"""'), Leaf(4, "\n")]),
            Node(simple_stmt, [Leaf(1, "pass"), Leaf(4, "\n")]),
            Leaf(6, ""),
        ],
    ),
]
[
    Leaf(1, "def"),
    Leaf(1, "g"),
    Node(parameters, [Leaf(7, "("), Leaf(1, "self"), Leaf(8, ")")]),
    Leaf(11, ":"),
    Node(
        suite,
        [
            Leaf(4, "\n"),
            Leaf(5, "        "),
            Node(simple_stmt, [Leaf(1, "pass"), Leaf(4, "\n")]),
            Leaf(6, ""),
        ],
    ),
]
[
    Leaf(1, "def"),
    Leaf(1, "foo"),
    Node(
        parameters,
        [
            Leaf(7, "("),
            Node(
                typedargslist,
                [
                    Leaf(1, "x"),
                    Leaf(12, ","),
                    Leaf(1, "y"),
                    Leaf(12, ","),
                    Leaf(1, "z"),
                ],
            ),
            Leaf(8, ")"),
        ],
    ),
    Leaf(55, "->"),
    Leaf(1, "int"),
    Leaf(11, ":"),
    Node(
        suite,
        [
            Leaf(4, "\n"),
            Leaf(5, "    "),
            Node(simple_stmt, [Leaf(3, '"""after"""'), Leaf(4, "\n")]),
            Node(simple_stmt, [Leaf(1, "pass"), Leaf(4, "\n")]),
            Leaf(6, ""),
        ],
    ),
]
[
    Leaf(1, "def"),
    Leaf(1, "boo"),
    Node(parameters, [Leaf(7, "("), Leaf(1, "x"), Leaf(8, ")")]),
    Leaf(55, "->"),
    Leaf(1, "int"),
    Leaf(11, ":"),
    Node(
        suite,
        [
            Leaf(4, "\n"),
            Leaf(5, "    "),
            Node(simple_stmt, [Leaf(3, '"""after"""'), Leaf(4, "\n")]),
            Node(simple_stmt, [Leaf(1, "pass"), Leaf(4, "\n")]),
            Leaf(6, ""),
        ],
    ),
]
[
    Leaf(1, "def"),
    Leaf(1, "f"),
    Node(parameters, [Leaf(7, "("), Leaf(1, "self"), Leaf(8, ")")]),
    Leaf(11, ":"),
    Node(
        suite,
        [
            Leaf(4, "\n"),
            Leaf(5, "        "),
            Node(simple_stmt, [Leaf(3, '"""after"""'), Leaf(4, "\n")]),
            Node(simple_stmt, [Leaf(1, "pass"), Leaf(4, "\n")]),
            Leaf(6, ""),
        ],
    ),
]
[
    Leaf(1, "def"),
    Leaf(1, "g"),
    Node(parameters, [Leaf(7, "("), Leaf(1, "self"), Leaf(8, ")")]),
    Leaf(11, ":"),
    Node(
        suite,
        [
            Leaf(4, "\n"),
            Leaf(5, "        "),
            Node(simple_stmt, [Leaf(3, '"""after"""'), Leaf(4, "\n")]),
            Node(simple_stmt, [Leaf(1, "pass"), Leaf(4, "\n")]),
            Leaf(6, ""),
        ],
    ),
]
