```console
$ python 01*.py --src data.yaml --schema schema.yaml
E '20' is not of type 'integer'

Failed validating 'type' in schema['properties']['person']['properties']['age']:
    {'type': 'integer'}

On instance['person']['age']:
    '20'
----------------------------------------
in "data.yaml", line 3, column 8
  01:    person:
  02:      name: foo
  03: ->   age: "20"  # ng
  04:    
```

元々のエラー

```console
$ python 00*.py --src data.yaml --schema schema.yaml
E '20' is not of type 'integer'

Failed validating 'type' in schema['properties']['person']['properties']['age']:
    {'type': 'integer'}

On instance['person']['age']:
    '20'
```

