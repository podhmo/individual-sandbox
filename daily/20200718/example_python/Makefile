FILE = $(shell echo $@*.py)

00:
	python $(FILE) | sort | uniq -c
01:
	mypy --strict $(FILE)
02:
	mypy --strict $(FILE)
