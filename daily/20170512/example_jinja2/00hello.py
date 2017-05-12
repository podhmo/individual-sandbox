from jinja2 import Template

t = Template("hello {{name}}.")
print(t.render(name="world"))
