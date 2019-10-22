00:
	rm -f $@*/example.db
	cd $@*/ && python main.py
01:
	rm -f $@*/example.db
	cd $@*/ && python -m unittest -v test*.py
02:
	rm -f $@*/example.db
	cd $@*/ && python main.py
03:
	python $@*/main.py
setup:
	pip install -r requirements.txt -U
