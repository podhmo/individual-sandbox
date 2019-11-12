00:
	python $@*.py --expose | tee 01hello.py
01: 00
02:
	python $@*.py -h
03:
	COLUMNS=1000 python $@*.py -h
04:
	python $@*.py --expose
05:
	python -m handofcats $$(echo $@*.py):run --expose
08:
	python $@*.py --expose
09:
	python $@*.py --expose
