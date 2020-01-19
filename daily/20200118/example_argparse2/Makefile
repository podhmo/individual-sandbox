00:
	python $(shell echo $@*.py) --config='{"main": {"db": "sqlite://:memory:"}}'
	python $(shell echo $@*.py) --config=file://config.json
	python $(shell echo $@*.py) --config=file://config-ng.json
	python $(shell echo $@*.py) --config=file://missing.json
01:
	python $(shell echo $@*.py) --config='{"main": {"db": "sqlite://:memory:"}}'
	python $(shell echo $@*.py) --config=file://config.json
	python $(shell echo $@*.py) --config=file://config-ng.json || exit 0
	python $(shell echo $@*.py) --config=file://missing.json
02:
	python $(shell echo $@*.py) --config=file://config.json
	python $(shell echo $@*.py) --config='{"main": {"db": "sqlite://:memory:"}}'
	python $(shell echo $@*.py) --config=file://config-ng.json || exit 0
	python $(shell echo $@*.py) --config=file://missing.json || exit 0
	python $(shell echo $@*.py) --show-schema
03:
	python $(shell echo $@*.py) --config='{"main": {"db": "sqlite://:memory:"}}'
	python $(shell echo $@*.py) --config=file://config.json
	python $(shell echo $@*.py) --config=file://config-ng.json

diff:
	diff -u 01*.py 02*.py > 0102.diff || exit 0
	diff -u 02*.py 03*.py > 0203.diff || exit 0
.PHONY: diff
