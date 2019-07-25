default:
	pip install ./foo
	$(MAKE) 00 01
	pip uninstall foo

00:
	python $@*.py | tee $@.output
01:
	python $@*.py | tee $@.output
