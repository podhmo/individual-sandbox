## json のserializeの速度

https://qiita.com/jagio/items/e3b2f9af0ee5096011ee

00

```
{"id":1,"name":"akane","birthday":"08-16","vivid_info":{"color":"red","weapon":"Rang"}}
```

```
go test -bench=. -benchmem
goos: linux
goarch: amd64
BenchmarkSerialize-4   	 2000000	       750 ns/op	     208 B/op	       2 allocs/op
PASS
ok  	~/my/individual-sandbox/daily/20180203/example_json/00person	2.251s
```

01

```
{"id": 1, "name": "akane", "birthday": "08-16", "vivid_info": {"color": "red", "weapon": "Rang"}}
```

```
go test -bench=. -benchmem
goos: linux
goarch: amd64
BenchmarkSerialize-4   	 1000000	      1500 ns/op	     528 B/op	      27 allocs/op
PASS
ok  	~my/individual-sandbox/daily/20180203/example_json/01person	1.524s
```
