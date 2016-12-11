from jsonspec.validators import load

# data will validate against this schema
validator = load({
    'title': 'Example Schema',
    'type': 'object',
    'properties': {
        'age': {
            'description': 'Age in years',
            'minimum': 0,
            'type': 'integer'
        },
        'firstName': {
            'type': 'string'
        },
        'lastName': {
            'type': 'string'
        }
    },
    'required': [
        'firstName',
        'lastName'
    ]
})

# validate this data
validator.validate({
    'firstName': 'John',
    'lastName': 'Noone',
    'age': 33,
})
