from prestring.python.parse import parse_file, PyTreeDumper
PyTreeDumper().visit(parse_file("hello.py"))
# file_input [2 children]
#   funcdef[name='hello'] [5 children]
#     NAME('def') [lineno=1, column=0, prefix='']
#     NAME('hello') [lineno=1, column=4, prefix=' ']
#     parameters [2 children]
#       LPAR('(') [lineno=1, column=9, prefix='']
#       RPAR(')') [lineno=1, column=10, prefix='']
#     COLON(':') [lineno=1, column=11, prefix='']
#     suite [7 children]
#       NEWLINE('\n') [lineno=1, column=12, prefix='']
#       INDENT('    ') [lineno=2, column=0, prefix='']
#       simple_stmt [2 children]
#         expr_stmt [3 children]
#           power [2 children]
#             NAME('hello') [lineno=2, column=4, prefix='']
#             trailer [2 children]
#               DOT('.') [lineno=2, column=9, prefix='']
#               NAME('__name__') [lineno=2, column=10, prefix='']
#           EQUAL('=') [lineno=2, column=19, prefix=' ']
#           STRING('"hellohello"') [lineno=2, column=21, prefix=' ']
#         NEWLINE('\n') [lineno=2, column=33, prefix='']
#       simple_stmt [2 children]
#         expr_stmt [3 children]
#           NAME('x') [lineno=3, column=4, prefix='    ']
#           EQUAL('=') [lineno=3, column=6, prefix=' ']
#           STRING('"world"') [lineno=3, column=8, prefix=' ']
#         NEWLINE('\n') [lineno=3, column=15, prefix='']
#       simple_stmt [2 children]
#         comparison [3 children]
#           NAME('x') [lineno=4, column=4, prefix='    ']
#           EQEQUAL('==') [lineno=4, column=6, prefix=' ']
#           NAME('x') [lineno=4, column=9, prefix=' ']
#         NEWLINE('\n') [lineno=4, column=10, prefix='']
#       simple_stmt [2 children]
#         power [2 children]
#           NAME('print') [lineno=5, column=4, prefix='    ']
#           trailer [3 children]
#             LPAR('(') [lineno=5, column=9, prefix='']
#             arglist [3 children]
#               STRING('"hello"') [lineno=5, column=10, prefix='']
#               COMMA(',') [lineno=5, column=17, prefix='']
#               NAME('x') [lineno=5, column=19, prefix=' ']
#             RPAR(')') [lineno=5, column=20, prefix='']
#         NEWLINE('\n') [lineno=5, column=21, prefix='']
#       DEDENT('') [lineno=6, column=0, prefix='']
#   ENDMARKER('') [lineno=6, column=0, prefix='']

