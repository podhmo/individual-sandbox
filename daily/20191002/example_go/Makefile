update:
	mkdir -p dst
	$(MAKE) -s 07 | tee dst/user.go
	diff -u target/user.go dst/user.go > a.diff

00:
	go run $@*/main.go testing
01:
	go run $@*/main.go testing
02:
	go run $@*/main.go testing
03:
	go run $@*/main.go m/target
04:
	go run $@*/main.go m/target
05:
	go run $@*/main.go m/target
06:
	go run $@*/main.go m/target
07:
	go run $@*/main.go m/target
