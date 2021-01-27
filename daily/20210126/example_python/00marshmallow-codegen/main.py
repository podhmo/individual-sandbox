"""
Generated Marshmallow models crash with TypeError: '<' not supported between instances of 'list' and 'int'
When OpenAPI uses minItems on an array definition
"""


import tempfile
import subprocess


OPENAPI_SPEC = """
openapi: 3.0.0
components:
  schemas:
    ExampleArray:
      description: Example array requiring at least one element
      type: array
      items:
        type: string
      minItems: 1
    ExampleSchema:
      type: object
      properties:
        array:
          $ref: "#/components/schemas/ExampleArray"
      required:
        - array
      additionalProperties: false
"""


def main() -> None:
    """
    Generate Marshmallow models from OpenAPI spec and attempt to use it
    """

    with tempfile.NamedTemporaryFile("w", encoding="utf-8") as temp_fh:
        temp_fh.write(OPENAPI_SPEC.strip())
        temp_fh.seek(0)
        output = subprocess.check_output(["swagger-marshmallow-codegen", temp_fh.name])

    output_str = str(output, "utf-8")

    print()
    print("Generated marshmallow model:")
    print()
    print(output_str)
    print()
    print("Please note validate=[ItemsRange(min=1 is probably incorrect")
    print("It should be using Length validator:")
    print(
        "https://marshmallow.readthedocs.io/en/stable/marshmallow.validate.html?highlight=length#marshmallow.validate.Length"
    )
    print()

    with open("generated_marshmallow_models.py", "w", encoding="utf-8") as path_fh:
        path_fh.write(output_str)

    print("Using the generated model with proper payload is now going to crash")
    print("with TypeError: '<' not supported between instances of 'list' and 'int'")
    print()
    print('Payload is {"array": ["string1", "string2"]}')
    print()

    from generated_marshmallow_models import ExampleSchema

    schema = ExampleSchema()
    schema.load({"array": ["string1", "string2"]})


if __name__ == "__main__":

    main()
