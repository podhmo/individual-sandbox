00:
	python $@*/run.py -c $@*/foo.json -x 10 -y 20
	python $@*/run.py -c $@*/bar.json -z 30
	python $@*/run.py -c $@*/bar.json -x 10 -y 20 || exit 0
	python $@*/run.py -c $@*/boo.json || exit 0
	python $@*/run.py -h
00-x:
	python 00*/run.py -c 00*/bar.json -x 10 -y 20 || exit 0
	python 00*/run.py -c 00*/boo.json || exit 0
01:
	python $@*/run.py -c $@*/foo.json -x 10 -y 20
	python $@*/run.py -c $@*/bar.json -z 30
	python $@*/run.py -c $@*/bar.json -x 10 -y 20 || exit 0
	python $@*/run.py -c $@*/boo.json || exit 0
	python $@*/run.py -h	
02:
	python $@*.py xxx
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
