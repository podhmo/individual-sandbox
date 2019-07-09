run: CONT ?=
run:
	pytest test_serial_speed.py --log-cli-level=INFO -s ${CONT}

run-with-log:
	$(MAKE) run CONT="|& tee run.log"

# setup environment
setup:
	pip install pytest fastapi pydantic faker requests timing_asgi
