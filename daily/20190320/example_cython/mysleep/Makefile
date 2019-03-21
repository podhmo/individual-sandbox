build: clean
	python setup.py build_ext --inplace
clean:
	rm -f *.so *.c

run:
	PYTHONPATH=. python -c 'import mysleep; print(mysleep.mysleep("OK", 1))'

