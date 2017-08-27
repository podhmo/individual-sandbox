code = """
L = range(10)
r = [x for x in L if x > i]
print(r)
""".strip()
env = {"i": 3}
exec(code, env)
print(env["r"])
