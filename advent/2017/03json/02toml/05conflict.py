import toml

s = """
name = "foo"
age = 20
name = "bar"
"""

d = toml.loads(s)
