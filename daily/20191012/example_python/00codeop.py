import codeop

source = """
def hello() -> str:
    return "hello"
"""

codeob = codeop.compile_command(source, filename="hello.py")
env = {}
exec(codeob, env)
print(env["hello"], env["hello"]())

# 複数行は対応していないっぽい  symbol="single" なので
