default: 00 01 02 03 04 05 06 07

00:
	python 00*.py ./target.py | tee $@.output
01:
	python 01*.py ./target.py | tee $@.output
02:
	python 00*.py marshmallow.schema | tee $@.output
03:
	python 01*.py $$(pyinspect resolve marshmallow.schema) | tee $@.output
04:
	python 00*.py starlette.datastructures | tee $@.output
05:
	python 01*.py $$(pyinspect resolve starlette.datastructures) | tee $@.output
06:
	python 00*.py argparse | tee $@.output
07:
	python 01*.py $$(pyinspect resolve argparse) | tee $@.output
