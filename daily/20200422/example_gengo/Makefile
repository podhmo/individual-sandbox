00:
	python $(shell echo $@*.py) --expose | black - | tee output.py
	python $(shell echo $@*.py) --name foo
	python output.py --name foo
