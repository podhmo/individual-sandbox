code = """
def f():
    pass

print("@", f.__module__, "@")
"""

exec(code, {"__name__": "<exec>"})
