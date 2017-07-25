import ast

t = ast.parse("""# %matplotlib inline""")
print(ast.dump(t))
