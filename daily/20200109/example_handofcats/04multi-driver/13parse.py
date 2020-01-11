from prestring.python.parse import parse_string, PyTreeDumper, PyTreeVisitor, type_repr

t = parse_string(
    """
aaa = bbb()
ccc.ddd = eee()
fff = ggg.hhh()

if __name__ == "__main__":
    main()
"""
)
PyTreeDumper().visit(t)


class Visitor(PyTreeVisitor):
    def visit_expr_stmt(self, node):
        print("@", node.children, len(node.children))
        print(node, "--", str(node.children[0]), "--")


Visitor().visit(t)

# file_input [4 children]
#   simple_stmt [2 children]
#     expr_stmt [3 children]
#       NAME('aaa') [lineno=2, column=0, prefix='\n']
#       EQUAL('=') [lineno=2, column=4, prefix=' ']
#       power [2 children]
#         NAME('bbb') [lineno=2, column=6, prefix=' ']
#         trailer [2 children]
#           LPAR('(') [lineno=2, column=9, prefix='']
#           RPAR(')') [lineno=2, column=10, prefix='']
#     NEWLINE('\n') [lineno=2, column=11, prefix='']
#   simple_stmt [2 children]
#     expr_stmt [3 children]
#       power [2 children]
#         NAME('ccc') [lineno=3, column=0, prefix='']
#         trailer [2 children]
#           DOT('.') [lineno=3, column=3, prefix='']
#           NAME('ddd') [lineno=3, column=4, prefix='']
#       EQUAL('=') [lineno=3, column=8, prefix=' ']
#       power [2 children]
#         NAME('eee') [lineno=3, column=10, prefix=' ']
#         trailer [2 children]
#           LPAR('(') [lineno=3, column=13, prefix='']
#           RPAR(')') [lineno=3, column=14, prefix='']
#     NEWLINE('\n') [lineno=3, column=15, prefix='']
#   simple_stmt [2 children]
#     expr_stmt [3 children]
#       NAME('fff') [lineno=4, column=0, prefix='']
#       EQUAL('=') [lineno=4, column=4, prefix=' ']
#       power [3 children]
#         NAME('ggg') [lineno=4, column=6, prefix=' ']
#         trailer [2 children]
#           DOT('.') [lineno=4, column=9, prefix='']
#           NAME('hhh') [lineno=4, column=10, prefix='']
#         trailer [2 children]
#           LPAR('(') [lineno=4, column=13, prefix='']
#           RPAR(')') [lineno=4, column=14, prefix='']
#     NEWLINE('\n') [lineno=4, column=15, prefix='']
#   ENDMARKER('') [lineno=5, column=0, prefix='']
