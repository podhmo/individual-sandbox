import jinja2

t = jinja2.Template("{{foo or 'none'}}")
print(t.render(foo="bar"))  # bar
print(t.render())  # none

t = jinja2.Template("{{foo is defined }}")
print(t.render())
