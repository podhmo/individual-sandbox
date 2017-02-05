```
$ make
PYTHONPATH=. python app/__init__.py
```

```
$ make test
bash test.sh
+ http PUT :8080/pets/1 name=foo animal_type=test
{"title": "Created", "code": "201 Created", "message": "\n\n\n\n\n"}+ http :8080/pets/1
{"animal_type": "test", "id": "1", "name": "foo", "created": "2017-02-05T16:07:38.310513"}+ http PUT :8080/pets/1 name=foo animal_type=test 'tags:={"color": "brown"}'
{"title": "OK", "code": "200 OK", "message": "\n\n\n\n\n"}+ http :8080/pets/1
{"animal_type": "test", "name": "foo", "id": "1", "tags": {"color": "brown"}, "created": "2017-02-05T16:07:38.310513"}+ http :8080/pets animal_type==test
[{"animal_type": "test", "name": "foo", "id": "1", "tags": {"color": "brown"}, "created": "2017-02-05T16:07:38.310513"}]+ http DELETE :8080/pets/1
```
