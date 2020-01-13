00:
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) foo
	python $(shell echo $@*.py) foo --nick-name F
	python $(shell echo $@*.py) --expose | tee exposed.py

02:
	handofcats $(shell echo $@*.py) --expose | tee exposed.py


03:
	python $(shell echo $@*.py) hello foo
	python $(shell echo $@*.py) hello foo --nick-name F
	python $(shell echo $@*.py) hello -h
	python $(shell echo $@*.py) byebye foo bar boo
	python $(shell echo $@*.py) byebye -h
	python $(shell echo $@*.py) --expose | tee exposed.py

04:
	handofcats $(shell echo $@*.py) -h
	handofcats $(shell echo $@*.py) hello -h
	handofcats $(shell echo $@*.py) byebye -h
	handofcats $(shell echo $@*.py) --expose | tee exposed.py

05:
	python $(shell echo $@*.py) -h
	python $(shell echo $@*.py) --expose
06:
	python $(shell echo $@*.py)
