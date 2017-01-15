```bash
$ pip install hug uwsgi
```

## how to use with command line interface

listing

```bash
$ make list
hug -f app.py -c list_people
{'name': 'foo', 'age': '20'}
{'name': 'bar', 'age': '20'}
```

create

```
$ NAME=boo AGE=20 make create
hug -f app.py -c create_person boo 20
$ make list
hug -f app.py -c list_people
{'name': 'foo', 'age': '20'}
{'name': 'bar', 'age': '20'}
{'name': 'boo', 'age': '20'}
```

## how to use with web interface

server

```
$ make server
```

api

```bash
$ http --pretty=format GET http://localhost:8000/people
[
    {
        "age": "20", 
        "name": "foo"
    }, 
    {
        "age": "20", 
        "name": "bar"
    }, 
    {
        "age": "20", 
        "name": "boo"
    }
]
$ http POST http://localhost:8000/people/ name=foo age=100
```

default is form data api.

### to rest json api

```bash
$ echo '{"name": "boo", "age": 10}' | http --json --verbose POST http://localhost:8000/people
```

for detail

- http://www.hug.rest/website/learn/directives
- https://github.com/timothycrosley/hug/blob/develop/examples/post_body_example.py

## examples

https://github.com/timothycrosley/hug/tree/develop/examples
