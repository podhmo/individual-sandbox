default: \
 00 01 02 03 04 05 \
 14 15 16

run: TARGET ?= xx
run:
	go run ${TARGET}*/main.go | tee $$(echo ${TARGET}*/)output.txt
	@echo "----------------------------------------"
	@cat -n ${TARGET}*/main.go | head -n $$(( $$(cut -d : -f 1 $$(echo ${TARGET}*/)output.txt) + 1 )) | tail -n 3

# .DEFAULT_GOAL := 00

00:
	$(MAKE) run TARGET=$@
01:
	$(MAKE) run TARGET=$@
02:
	$(MAKE) run TARGET=$@
03:
	$(MAKE) run TARGET=$@
04:
	$(MAKE) run TARGET=$@
05:
	$(MAKE) run TARGET=$@

13:
	$(MAKE) run TARGET=$@
14:
	$(MAKE) run TARGET=$@
15:
	$(MAKE) run TARGET=$@
16:
	$(MAKE) run TARGET=$@

20:
	go run $@*/main.go
