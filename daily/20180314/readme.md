## elastic search

```console
http --json DELETE http://localhost:9200/values
http --json DELETE http://localhost:9200/values2
cat mapping.json | http --json PUT http://localhost:9200/values
cat mapping.json | http --json PUT http://localhost:9200/values2
python insert_elastic2.py
python insert_elastic3.py
```
