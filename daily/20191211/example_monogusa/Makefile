00:
	python -m monogusa.cli  $@*.py hello --name="world"
01:
	uvicorn --debug $(shell echo $@* | sed 's/\.py$$//'):app --port 44444

# use apistar?
01-client:
	echo '{}' | http --json POST :44444/hello

02:
	python $@*.py
03:
	python $@*.py
