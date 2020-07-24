# 今まではこう書いていた
00:
	python $(shell echo $@*.py) | sort | uniq -c
01:
	mypy --strict $(shell echo $@*.py)
