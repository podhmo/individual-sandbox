from jinja2 import Environment


def oops(value):
    return "{}!!".format(value)

env = Environment()
env.filters["oops"] = oops

t = env.from_string("hello, {{getname()|oops}}")
print(t.render(getname=lambda: "world"))
