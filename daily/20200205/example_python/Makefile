00:
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py)

01:
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) foo -h
	python $(shell echo $@*.py) foo
	python $(shell echo $@*.py) bar
02:
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) sqs -h
	python $(shell echo $@*.py) sqs list -h
	python $(shell echo $@*.py) s3 -h
	python $(shell echo $@*.py) s3 cp -h
03:
	mypy --strict $(shell echo $@*.py)
