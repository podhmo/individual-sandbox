from jinja2 import Template

t = Template("@{{foo}}@")
print(t.render(**{"foo": "bar"}))

t = Template("@{{foo['bar']}}@")
print(t.render(**{"foo": {"bar": "boo"}}))

# with json...

t = Template("""
{
  "name": "{{name}}"
}
""")

print(t.render(name="foo"))
