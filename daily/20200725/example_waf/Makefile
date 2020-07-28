00:
	uvicorn --port=4444 --reload $(patsubst %.py,%,$(shell echo $@*)):app
01:
	PORT=4444 python $(shell echo $@*.py)
