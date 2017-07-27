import parselib as p
import buildlib as b

code = """
def add(x, y, z=0):
    ans = x + y + z
    return ans
"""

# todo: indentation

t = b.File(
    b.Def(
        "add",
        b.Args("x", "y", z=b.Number(0)),
        b.Node(
            b.syms.simple_stmt, [
                b.Node(
                    b.syms.expr_stmt, [
                        b.Assign(
                            b.Name("ans"),
                            b.Node(
                                b.syms.arith_expr, [
                                    b.Name("x"),
                                    b.Leaf(b.token.PLUS, "+", prefix=" "),
                                    b.Name("y", prefix=" "),
                                    b.Leaf(b.token.PLUS, "+", prefix=" "),
                                    b.Name("z", prefix=" "),
                                ]
                            )
                        )
                    ]
                ),
                b.Newline(),
            ]
        ),
        b.Node(
            b.syms.simple_stmt,
            [
                b.Node(
                    b.syms.return_stmt,
                    [b.Name("return", prefix="    "), b.Name("ans", prefix=" ")]
                ),
                b.Newline(),
            ],
        ),
    )
)
# t = p.parse_from_string(code)
p.dump_tree(t)
print(t)
