import yaml

person = {"name": "foo", "age": 20}
print(yaml.dumps(person))

# list
print(yaml.dumps([person]))
