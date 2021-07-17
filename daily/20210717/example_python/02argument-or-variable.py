import dis

def inc(v:int) -> int:
    d = 1
    r = v + d
    return r
dis.dis(inc)