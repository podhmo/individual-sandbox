from dictknife import loading
from schema import Namespace, Object, Array, get_resolver


class Person(Object):
    """person"""

    name: str
    age: int


class XPerson(Object):
    """X person"""
    x: str
    person: Person


class People(Array):
    items = Person


# todo: default,example,validation
# todo: string as type (order of reference)
with Namespace("components") as components:
    with components.namespace("schemas") as schemas:
        # schemas.mount(Person)
        schemas.mount(People)
        schemas.mount(XPerson)
    assert get_resolver().lookup.lookup(components, "schemas/Person") is not None
    loading.dumpfile(components.as_dict(), format="json")

# {
#   "components": {
#     "schemas": {
#       "Person": {
#         "type": "object",
#         "description": "person",
#         "properties": {
#           "name": {
#             "type": "string"
#           },
#           "age": {
#             "type": "integer"
#           }
#         },
#         "required": [
#           "name",
#           "age"
#         ]
#       },
#       "People": {
#         "type": "array",
#         "items": {
#           "$ref": "#/components/schemas/Person"
#         }
#       },
#       "XPerson": {
#         "type": "object",
#         "description": "X person",
#         "properties": {
#           "x": {
#             "type": "string"
#           },
#           "person": {
#             "$ref": "#/components/schemas/Person"
#           }
#         },
#         "required": [
#           "x",
#           "person"
#         ]
#       }
#     }
#   }
# }
