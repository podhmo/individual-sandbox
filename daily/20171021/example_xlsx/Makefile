XLSFILE = $(shell python -c 'import xlrd; import os.path; print(os.path.join(os.path.dirname(xlrd.__file__), "examples/namesdemo.xls"))')
XLSFILE = test.xlsx

00:
	time python 00tocsv.py ${XLSFILE} 00.csv

01:
	python 01*.py
