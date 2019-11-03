setup:
	pip install prestring

00:
	python $@*.py $@foo
01:
	python $@*.py $@foo
02:
	python $@*.py $@foo --config config.json
03:
	mkdir -p $@text
	python $@*.py | tee $@text/gen.py
	python $@text/gen.py | tee $@text/output.go
	diff -u $@text/main.go.template $@text/output.go
