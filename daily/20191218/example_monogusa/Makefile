00:
	# as cli
	python $(shell echo $@*/)commands.py hello
	# as web
	python $(shell echo $@*/)web.py --show-doc
00-serve:
	python 00*/web.py --port 55555
00-client:
	echo '{}' | http --json POST :55555/hello
01:
	python $(shell echo $@*/)gen.py
02:
	python $@*.py
03:
	python $@*.py
04:
	python $@*.py
05:
	python $@*.py
06:
	python $@*.py
07:
	python $@*.py
08:
	python $@*.py
09:
	python $@*.py
10:
	python $@*.py
11:
	python $(shell echo $@*/)codegen.py | tee $(shell echo $@*/)routers.py
12:
	python -m monogusa.web --with-main $(shell echo $@*/)commands.py | sed "s@$(shell pwd)/@@g" | tee $(shell echo $@*/)web.py
	# python $(shell echo $@*/)web.py --show-doc
