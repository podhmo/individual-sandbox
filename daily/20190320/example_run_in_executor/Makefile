build: clean
	python setup.py build_ext --inplace
check:
	PYTHONPATH=. python -c 'import mysleep; print(mysleep.mysleep("OK", 1))'

run:
	PYTHONPATH=. python 00use_thread_pool_executor.py
	PYTHONPATH=. python 01use_process_pool_executor.py

clean:
	rm -f *.so *.c


