00:
	dictknife cat 00*.json -o csv | tee 00.csv
01:
	dictknife cat 00*.json -o md | tee 01.md
02:
	python 02*.py | tee 02.txt
03:
	dictknife transform --function dictknife.transform:flatten --src 02person.json -o md | tee 03.md
04:
	dictknife --debug transform --function flatten --src 02person.json -o md | tee 04.md
05:
	dictknife --debug transform --function flatten --function only_number --src 02person.json -o md | tee 05.md
