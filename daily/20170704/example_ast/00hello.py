import ast

t = ast.parse("print('hello')")
print(ast.dump(t))
