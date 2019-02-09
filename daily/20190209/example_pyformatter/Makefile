default: 00 01 02

setup:
	pip install po-yayapf black autopep8

00:
	cp 00hello.py black.00.py
	black black.00.py
01:
	cp 00hello.py yayapf.00.py
	yayapf -i yayapf.00.py
02:
	cp 00hello.py autopep8.00.py
	autopep8 -i -a autopep8.00.py
