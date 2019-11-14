00:
	python $@*.py |& tee $@.output
01:
	python $@*.py |& tee $@.output
02:
	python $@*.py |& tee $@.output
03:
	python $@*.py |& tee $@.output

04: left ?= before.py
04: right ?= after.py
04: output ?= $@.output
04:
	python $@*.py --left $(left) --right $(right) |& tee $(output)
05:
	$(MAKE) 04 left=04.output output=$@.output
	$(MAKE) 04 left=$@.output output=06.output
	diff -u 04.output 05.output || exit 0
	diff -u 05.output 06.output || exit 0
