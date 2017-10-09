def evaluate(code, *, env):
    if not isinstance(code, (list, tuple)):
        return code
    op = code[0]
    if op == "set":
        env[code[1]] = evaluate(code[2], env=env)
    elif op == "get":
        return env[code[1]]
    elif op == "+":
        return evaluate(code[1], env=env) + evaluate(code[2], env=env)
    elif op == "=":
        return evaluate(code[1], env=env) == evaluate(code[2], env=env)
    elif op == "step":
        r = None
        for val in code[1:]:
            r = evaluate(val, env=env)
        return r
    elif op == "until":
        while not evaluate(code[1], env=env):
            evaluate(code[2], env=env)
    else:
        raise RuntimeError(code)


code = [
    "step", ["set", "i", 10], ["set", "sum", 0], [
        "until", ["=", ["get", "i"], 0], [
            "step", ["set", "sum", ["+", ["get", "sum"], ["get", "i"]]],
            ["set", "i", ["+", ["get", "i"], -1]]
        ]
    ], ["get", "sum"]
]

print(evaluate(code, env={}))
