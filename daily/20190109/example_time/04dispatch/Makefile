TRACE ?=
ifeq ($(TRACE),)
	outOP := &>
else
	outOP := |& tee
endif

default: clean
	(time sleep 1) ${outOP} aaa
	(time sleep 1) ${outOP} bbb

clean:
	rm -f aaa bbb

