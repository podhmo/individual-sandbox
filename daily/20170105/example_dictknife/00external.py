from dictknife import LooseDictWalker

d = {
    "definitions": {
        "name": {
            "type": "string",
            "description": "name of someone"
        },
        "age": {
            "type": "integer",
            "minimum": 0
        },
        "person": {
            "properties": {
                "name": {
                    "$ref": "#/definitions/name"
                },
                "age": {
                    "$ref": "#/definitions/age"
                }
            }
        }
    }
}


itr = []


def on_has_ref(path, d):
    print("provide", d)
    itr.append(d["$ref"])

walker = LooseDictWalker(on_container=on_has_ref)
walker.walk(["$ref"], d)
for d in itr:
    print("consume", d)
