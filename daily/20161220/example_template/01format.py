s = "@%{foo}@"
print(s.format(**{"foo": "bar"}))

# error
s = "@%{foo[bar]}@"
print(s.format(**{"foo": {"bar": "boo"}}))

# with json...

data = """
{{
  "name": "{{name}}"
}}
"""

print(data.format(name="foo"))
