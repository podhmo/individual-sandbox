from dataclasses import dataclass
import os
import subprocess
import gc
import tempfile
import string
import json


@dataclass(frozen=True)
class Person:
    a: str
    b: str
    c: str
    d: str
    e: str
    f: str
    g: str
    h: str
    i: str
    j: str
    k: str
    l: str
    m: str
    n: str
    o: str
    p: str
    q: str
    r: str
    s: str
    t: str
    u: str
    v: str
    w: str
    x: str
    y: str
    z: str
    A: str
    B: str
    C: str
    D: str
    E: str
    F: str
    G: str
    H: str
    I: str
    J: str
    K: str
    L: str
    M: str
    N: str
    O: str
    P: str
    Q: str
    R: str
    S: str
    T: str
    U: str
    V: str
    W: str
    X: str
    Y: str
    Z: str


def ps():
    subprocess.run(["ps", "-o", "pid,time,pcpu,pmem,vsz,rss", "-p", str(os.getpid())], check=True, stderr=subprocess.STDOUT)


def write(wf):
    print("before write")
    d = {
        k: v
        for k, v in zip(string.ascii_letters, map((lambda s: s.upper()), string.ascii_letters))
    }
    L = [d] * 5 * (10 ** 4)
    json.dump(L, wf)
    gc.collect()
    ps()


def load_dict(rf):
    d = json.load(rf)
    gc.collect()
    ps()


def load_dataclass(rf):
    d = json.load(rf, object_hook=lambda d: Person(**d))
    gc.collect()
    ps()


gc.collect()
ps()
with tempfile.NamedTemporaryFile("w", delete=False) as wf:
    write(wf)
    print("----------------------------------------")
    print("after write")
    gc.collect()
    ps()

print("----------------------------------------")
print("load dataclass")
with open(wf.name) as rf:
    load_dataclass(rf)
gc.collect()
ps()

print("----------------------------------------")
print("load dataclass")
with open(wf.name) as rf:
    load_dataclass(rf)
gc.collect()
ps()

os.unlink(wf.name)

