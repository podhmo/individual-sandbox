00:
	go run $@*/main.go
00-client:
	http :8080/xxx?foo=bar&pretty=1

01:
	go run $@*/main.go
01-client:
	http :8080/xxx Foo:Bar

02:
	go run $@*/main.go
02-client:
	http -f POST :8080/xxx foo=Bar

03:
	go run $@*/main.go
03-client:
	echo '{"foo": "bar", "pretty": true}' | http --json POST :8080/xxx

04:
	go run $@*/main.go
04-client:
	http --pretty=none :8080/
	http --pretty=none :8080/ pretty=1

05:
	go run $@*/main.go
05-client:
	http :8080/

06:
	go run $@*/main.go
06-client:
	http :8080/
