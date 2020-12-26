from invoke import task
from invoke.context import Context


@task
def clean(c, docs=False, bytecode=False, extra=""):
    patterns = ["build"]
    if docs:
        patterns.append("docs/_build")
    if bytecode:
        patterns.append("**/*.pyc")
    if extra:
        patterns.append(extra)
    for pattern in patterns:
        c.run("echo rm -rf {}".format(pattern))


@task
def build(c, docs=False):
    c.run("echo python setup.py build")
    if docs:
        c.run("echo sphinx-build docs docs/_build")
