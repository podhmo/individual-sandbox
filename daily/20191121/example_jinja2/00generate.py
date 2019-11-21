import sys
from jinja2 import Environment
from functools import partial

env = Environment()
code = """
hello {{world}}
"""
t = env._parse(code, name="hello", filename="hello.j2")
source = env._generate(t, name="hello", filename="hello.j2")

perr = partial(print, file=sys.stderr)
perr("----------------------------------------")
perr("input")
perr("----------------------------------------")
perr(code)
perr("----------------------------------------")
perr("node")
perr("----------------------------------------")
perr(t)
perr("----------------------------------------")
perr("code")
perr("----------------------------------------")
sys.stderr.flush()
print(source)
