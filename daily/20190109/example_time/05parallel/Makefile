TRACE ?=
ifeq ($(TRACE),)
	outOP := &>
else
	outOP := |& tee
endif

default: clean
	$(MAKE) aaa bbb

aaa:
	(time sleep 1) ${outOP} aaa
bbb:
	(time sleep 1) ${outOP} bbb

clean:
	rm -f aaa bbb
.PHONY: aaa bbb
