# help
00:
	python cli.py -h
01:
	python cli.py hello -h
02:
	python cli.py byebye -h
# run
03:
	python cli.py hello
04:
	python cli.py hello --name foo
05:
	python cli.py byebye
06:
	python cli.py byebye foo
# codegen
07:
	python cli.py --expose | tee cli-exposed.py
08:
	python cli2.py --expose
09:
	python cli3.py --expose

10:
	python $(shell echo $@*.py)
11:
	python $(shell echo $@*.py)
12:
	python $(shell echo $@*.py)
