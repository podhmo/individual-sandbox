import ast

print(ast.dump(ast.parse('name = "world"; print(f"hello, {name}")')))
