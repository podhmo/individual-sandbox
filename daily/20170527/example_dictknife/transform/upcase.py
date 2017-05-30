def upcase(d):
    if hasattr(d, "keys"):
        return {upcase(k): upcase(v) for k, v in d.items()}
    elif isinstance(d, (list, tuple)):
        return [upcase(d) for e in d]
    else:
        return str(d).upper()
