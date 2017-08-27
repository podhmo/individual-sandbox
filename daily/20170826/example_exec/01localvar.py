code = """
i = 3
L = range(10)
print([x for x in L if x > i])
""".strip()

exec(code, {})
