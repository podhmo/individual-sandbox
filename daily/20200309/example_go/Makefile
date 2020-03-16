00:
	go run $(shell echo $@*/)main.go https://foo.bar.jp/xxx/yyy foo.bar.jp/xxx/yyy /xxx/yyy xxx/yyy | tee 00.output
	go run $(shell echo $@*/)main.go "https://foo.bar.jp/xxx/?yyy=1&zzz=2" "https://foo.bar.jp/xxx/?zzz=2&yyy=1"  | tee 00.qs.output
