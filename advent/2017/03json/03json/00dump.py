import yaml
import sys

# dict
person = {"name": "foo", "age": 20}
yaml.dump(person, sys.stdout, default_flow_style=False)
# age: 20
# name: foo

# list
yaml.dump([person], sys.stdout, default_flow_style=False)
# - age: 20
#   name: foo
