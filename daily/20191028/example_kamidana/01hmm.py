import jinja2

t = """\
1: # access undefined
2: 
3: {{xxx}} # <- accessing undefined variable, here."""

print(jinja2.__version__)
print(jinja2.Template(t, undefined=jinja2.StrictUndefined).render())
