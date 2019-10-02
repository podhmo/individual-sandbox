default:
	$(MAKE) 00 01
	diff -u 00*/output 01*/output | tee a.diff

00:
	go run $@*/main.go
	go tool compile -S $@*/main.go 2>&1 | sed 's@$(shell echo $@*)@D@g; s@${HOME}@HOME@g' | tee `echo $@*`/output

01:
	go run $@*/main.go
	go tool compile -S $@*/main.go 2>&1 | sed 's@$(shell echo $@*)@D@g; s@${HOME}@HOME@g' | tee `echo $@*`/output
