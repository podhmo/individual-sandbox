```console
$ python 17*.py person.json person-schema.yaml
? 0
----------------------------------------
     1  {
     2    "name": "foo",
     3    "age": 20,
     4    "father": {
     5      "name": "A",
     6      "age": 40
     7    },
     8    "mother": {
     9      "name": "B",
    10      "age": 40
    11    }
    12  }
$ python 17*.py ng-person.json person-schema.yaml
? 2
status:ERROR    cls:ValidationError     filename:ng-person.json start:4@12      end:6@3       msg:'name' is a required property (validator=required)  where:['ng-person.json:4']
status:ERROR    cls:ValidationError     filename:ng-person.json start:9@11      end:9@20      msg:'invalid' is not of type 'integer' (validator=type) where:['ng-person.json:9']
----------------------------------------
     1  {
     2    "name": "foo",
     3    "age": 20,
     4    "father": {
     5      "age": 40
     6    },
     7    "mother": {
     8      "name": "B",
     9      "age": "invalid"
    10    }
    11  }
```
