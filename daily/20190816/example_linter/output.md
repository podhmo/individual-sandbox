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
