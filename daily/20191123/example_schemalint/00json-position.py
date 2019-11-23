import typesystem


class Player(typesystem.Schema):
    name = typesystem.String()
    age = typesystem.Integer()


class Match(typesystem.Schema):
    left = typesystem.Reference(to=Player, allow_null=False)
    right = typesystem.Reference(to=Player, allow_null=False)


text = """{
  "left": {
    "name": "foo",
    "age": 20
  },
  "right": {
    "name": "bar",
    "age": "twenty"
  }
}
"""

try:
    print(typesystem.validate_json(text, validator=Match))
except typesystem.ValidationError as e:
    print(e)
    for message in e.messages():
        print(message)
