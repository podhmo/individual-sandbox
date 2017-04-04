default:
	yapf src.py > dst.py
	python bin/yyapf.py src.py > dst2.py
	diff -u dst.py dst2.py
