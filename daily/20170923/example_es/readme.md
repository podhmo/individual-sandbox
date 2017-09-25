starting elasticsearch

```
$ make middleware
docker-compose up
```

creating index and inserting data

```
$ make create
cat index.json | http --json -a elastic:changeme PUT :19200/values
bash gen.sh 100 | jqfpy --slurp 'h.chunk(get(), n=25)' --squash -c | (while read LINE; do echo $LINE | bash transform.sh | http -a elastic:changeme -b POST :19200/_bulk 'Content-Type:application/x-ndjson'; done)
```

check

```
$ make check
http -a elastic:changeme :19200/
http -b --json -a elastic:changeme ":19200/_cat/indices"
```

search

```
$ make search
http -b --json -a elastic:changeme ":19200/values/_search?q=*:*&pretty"
http -b --json -a elastic:changeme ":19200/values/_count?q=*:*&pretty"
```
