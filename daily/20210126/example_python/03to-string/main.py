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


