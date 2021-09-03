import re


def pyvalue(v):
    try:
        return float(v)
    except Exception:
        return v


s = "(速度)50 (音程)-100.2 (話者)男性1"
rx = re.compile(r"\(([^ )]+)\)([^ (]+)")
print({k: int(v) if v.lstrip("-").isdigit() else v for k, v in rx.findall(s)})
print({k: pyvalue(v) for k, v in rx.findall(s)})
