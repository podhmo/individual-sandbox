XLSFILE = $(shell python -c 'import xlrd; import os.path; print(os.path.join(os.path.dirname(xlrd.__file__), "examples/namesdemo.xls"))')
XLSFILE = test.xlsx

00:
	time python 00tocsv.py ${XLSFILE} 00.csv

01:
	python 01*.py

02:
	time python 02*.py 00.csv 02.csv

03:
	time kernprof -l 03tocsv.py ${XLSFILE} 00.csv
	python -m line_profiler 03*.lprof

04:
	time go run 04tocsv.go

