```console
$ python parse.py person.json
----------------------------------------
{'age': 20, 'name': 'foo', 'parents': [{'age': 40, 'name': 'A'}, {'age': 40, 'name': 'B'}]}
:   in "person.json", line 1, column 1   in "person.json", line 14, column 2
----------------------------------------
{'age': 40, 'name': 'A'}
:   in "person.json", line 5, column 5   in "person.json", line 8, column 6
----------------------------------------
     1	{
     2	  "age": 20,
     3	  "name": "foo",
     4	  "parents": [
     5	    {
     6	      "age": 40,
     7	      "name": "A"
     8	    },
     9	    {
    10	      "age": 40,
    11	      "name": "B"
    12	    }
    13	  ]
    14	}
```

after

```console
$ python parse.py a0.yaml
components:
  schemas:
    a:
      $ref: 'a1.yaml#/components/schemas/a'
      type: object
      properties:
        name:
          type: string
----------------------------------------
     1	components:
     2	  schemas:
     3	    a:
     4	      $ref: "a1.yaml#/components/schemas/a"
@   in "a0.yaml", line 4, column 7
@   in "a0.yaml", line 5, column 1
```

after (ng)

```console
$ python parse.py a0.yaml
   a0.yaml
     a1.yaml
       a2.yaml
         a3.yaml
           problem mapping values are not allowed here @   in "a3.yaml", line 7, column 15
           context None @ None

----------------------------------------
     1	components:
     2	  schemas:
     3	    a:
     4	      type: object
     5	      properties:
     6	        name
     7	          type: string
@   in "a0.yaml", line 4, column 7
@   in "a0.yaml", line 5, column 1
```
