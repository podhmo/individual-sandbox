shell 1

```console
$ make 00
sqlite> begin;
sqlite> update xxx set name 'xxx' where id = 1
```

shell 2
```console
$ make 00
sqlit> begin;
sqlite> update xxx set name 'xxx' where id = 2
Error: database is locked
```

see also

- https://www.antun.net/tips/api/sqlite.html
