## json のserializeの速度

https://qiita.com/jagio/items/e3b2f9af0ee5096011ee ここからjsonの定義借りてくる。

00(json.Encoder)

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

01(io.Writer.Write, convert to []byte)

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

02(fmt.Fprintf)

```
{"id": 1, "name": "akane", "birthday": "08-16", "vivid_info": {"color": "red", "weapon": "Rang"}}
```

```
go test -bench=. -benchmem
goos: linux
goarch: amd64
BenchmarkSerialize-4   	 2000000	       943 ns/op	     360 B/op	       7 allocs/op
PASS
ok  	~/my/individual-sandbox/daily/20180203/example_json/02person	2.840s
```

03 (io.WriteString)

```
{"id": 1, "name": "akane", "birthday": "08-16", "vivid_info": {"color": "red", "weapon": "Rang"}}
```

```
go test -bench=. -benchmem
goos: linux
goarch: amd64
BenchmarkSerialize-4   	 1000000	      1317 ns/op	     360 B/op	      12 allocs/op
PASS
ok  	~/my/individual-sandbox/daily/20180203/example_json/03person	1.337s
```

04 (io.Writestring without fmt.Sprintf)

```
{"id": 1, "name": "akane", "birthday": "08-16", "vivid_info": {"color": "red", "weapon": "Rang"}}
```

```
go test -bench=. -benchmem
goos: linux
goarch: amd64
BenchmarkSerialize-4   	 3000000	       480 ns/op	     256 B/op	       2 allocs/op
PASS
ok  	~/my/individual-sandbox/daily/20180203/example_json/04person	1.912s
```

05 (stringWriter, no io.WriteString's cast)

```
{"id": 1, "name": "akane", "birthday": "08-16", "vivid_info": {"color": "red", "weapon": "Rang"}}
```

```
go test -bench=. -benchmem
goos: linux
goarch: amd64
BenchmarkSerialize-4   	 5000000	       291 ns/op	     256 B/op	       2 allocs/op
PASS
ok  	_/home/nao/my/individual-sandbox/daily/20180203/example_json/05person	1.749s
```
