from pyramid.view import(
    view_config
)


@view_config(renderer='json', request_method='GET', route_name='app_views')
def get_pets(context, request):
    """
    Get all pets

    request.GET:

        * 'animal_type'  -  `{"type": "string", "pattern": "^[a-zA-Z0-9]*$"}`
        * 'limit'  -  `{"type": "integer", "minimum": 0, "default": 100}`
    """
    return {}


@view_config(renderer='json', request_method='GET', route_name='app_views1')
def get_pet(context, request):
    """
    Get a single pet

    request.matchdict:

        * 'pet_id'  Pet's Unique identifier  `{"type": "string", "required": true, "pattern": "^[a-zA-Z0-9-]+$"}`
    """
    return {}


@view_config(renderer='json', request_method='PUT', route_name='app_views1')
def put_pet(context, request):
    """
    Create or update a pet

    request.matchdict:

        * 'pet_id'  Pet's Unique identifier  `{"type": "string", "required": true, "pattern": "^[a-zA-Z0-9-]+$"}`

    request.json_body:

    ```
        {
          "type": "object",
          "required": [
            "name",
            "animal_type"
          ],
          "properties": {
            "id": {
              "type": "string",
              "description": "Unique identifier",
              "example": "123",
              "readOnly": true
            },
            "name": {
              "type": "string",
              "description": "Pet's name",
              "example": "Susie",
              "minLength": 1,
              "maxLength": 100
            },
            "animal_type": {
              "type": "string",
              "description": "Kind of animal",
              "example": "cat",
              "minLength": 1
            },
            "tags": {
              "type": "object",
              "description": "Custom tags"
            },
            "created": {
              "type": "string",
              "format": "date-time",
              "description": "Creation time",
              "example": "2015-07-07T15:49:51.230+02:00",
              "readOnly": true
            }
          }
        }
    ```
    """
    return {}


@view_config(renderer='json', request_method='DELETE', route_name='app_views1')
def delete_pet(context, request):
    """
    Remove a pet

    request.matchdict:

        * 'pet_id'  Pet's Unique identifier  `{"type": "string", "required": true, "pattern": "^[a-zA-Z0-9-]+$"}`
    """
    return {}
