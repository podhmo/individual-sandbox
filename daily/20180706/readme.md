## go go-cmp使って見る

```
go get -u github.com/google/go-cmp/cmp
```

できること

- diff https://godoc.org/github.com/google/go-cmp/cmp#example-Diff--Testing
- 

## isoformatの表示

shell

```
$ date +"%Y-%m-%dT%H:%M:%S%z"
2018-07-06T12:45:38+0900
```

python

```
>>> from datetime import datetime
>>> datetime.now().isoformat()
'2018-07-06T12:45:57.361555'
```

with tz

```
>>> import arrow
>>> arrow.now().isoformat()
'2018-07-06T12:52:34.044401+09:00'
```

(arrow.get()はutcっぽい)
