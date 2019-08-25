import jinja2

t = """
# access undefined

{{xxx}} # <- accessing undefined variable, here.
"""

print(jinja2.__version__)
print(jinja2.Template(t, undefined=jinja2.StrictUndefined).render())
