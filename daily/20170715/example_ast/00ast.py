import ast

with open("hello.py") as rf:
    t = ast.parse(rf.read())

# print(ast.dump(t))
from ppast import print_ast
print_ast(t)
