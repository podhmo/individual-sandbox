# parse
00:
	python -m prestring.python.parse main.py | tee $@.output
# transform
01:
	@python -m prestring.python main.py | tee $@.output
# parse
02:
	python -m prestring.python.parse main2.py | tee $@.output
# transform
03:
	@python -m prestring.python main2.py | tee $@.output
