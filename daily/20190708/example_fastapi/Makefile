benchmark: N ?= 00
benchmark:
	pytest --log-cli-level=INFO -s test_serial_speed.py |& sed "s@${HOME}@~@g" | tee ${N}benchmark.txt

sync:
	cp ~/venvs/fastapi/fastapi/fastapi/*.py $$(python -c 'import sys; print("\n".join(sys.path))' | grep site-packages)/fastapi/

setup:
	# python -m venv fastapi
	# cd fastapi; . bin/activate
	git clone git@github.com:tiangolo/fastapi.git
	cd fastapi; pip install .[test,dev]
	pip install faker timing-asgi
