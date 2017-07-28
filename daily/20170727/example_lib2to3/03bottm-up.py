import parselib as p
import buildlib as b

code = """
# toplevel comment
def add(x, y, z=0):
    # this is comment
    ans = x + y + z
    return ans
"""

# todo: indentation

t = b.File(
    b.Comment("# toplevel comment\n"),
    b.Def(
        "add",
        b.Args("x", "y", z=b.Number(0)),
        b.Comment("# toplevel comment\n"),
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
                    ],
                    prefix="    "
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
