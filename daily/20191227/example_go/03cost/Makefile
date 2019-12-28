00:
	$(MAKE) gen TARGET=nullbool
	$(MAKE) gen TARGET=nullbool2
	diff -u nullbool.output nullbool2.output > a.diff

01:
	$(MAKE) gen TARGET=val

gen: TARGET ?= ""
gen:
	(cd ${TARGET} && go build -gcflags -S 2>&1 ) | sed 's@$(shell pwd)@.@g; s/nullbool[0-9]/nullbool/g' | tee ${TARGET}.output
