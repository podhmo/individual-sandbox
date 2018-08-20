00:
	python 00*.py --input-file data.json
	cat data.json | python 00*.py
01:
	python 01*.py --input-file data.json
	cat data.json | python 01*.py
02:
	python 02*.py --input-file data.json
	cat data.json | python 02*.py
03:
	python 03*.py --input-file data.json
	cat data.json | python 03*.py
04:
	rm -rf output
	python 04*.py
	python 04*.py -f output/04.txt  # no such file or directory
05:
	rm -rf output
	python 05*.py
	python 05*.py -f output/05.txt  # ok (mkdir -p)
10:
	python 10*.py
	python 10*.py --output-file 10.json
	cat 10.json
11:
	python 11*.py
	python 11*.py --output-file 11.json
	cat 11.json

