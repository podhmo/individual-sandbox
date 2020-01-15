from typed_ast import ast3

code = """
def hello():
    name = "world"
    print(f"hello {world}")
"""

t = ast3.parse(code)
print(t)

print("----------------------------------------")
print(ast3.dump(t))

print("----------------------------------------")
for node in ast3.walk(t):
    print(node)
