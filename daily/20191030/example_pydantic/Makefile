# N= or N=2 or N=3
N ?= 

default:
	$(MAKE) -k 00 01 02 03

# use pydantic
00:
	# mypy --ignore-missing-imports --strict --strict-equality $@*/
	cd $@* && python dump.py | tee schema.yaml
	cd $@* && python run.py --config ../data/config.json
	cd $@* && python run.py --config ../data/config-ng${N}.json

# use typesystem
01:
	# mypy --ignore-missing-imports --strict --strict-equality $@*/
	cd $@* && python dump.py | tee schema.yaml
	cd $@* && python run.py --config ../data/config.json
	cd $@* && python run.py --config ../data/config-ng${N}.json

# use pydantic + snakecase
02:
	# mypy --ignore-missing-imports --strict --strict-equality $@*/
	cd $@* && python dump.py | tee schema.yaml
	cd $@* && python run.py --config ../data/config.json
	cd $@* && python run.py --config ../data/config-ng${N}.json

# use pydantic + snakecase + dataclasses
03:
	# mypy --ignore-missing-imports --strict --strict-equality $@*/
	# cd $@* && python dump.py | tee schema.yaml
	cd $@* && python run.py --config ../data/config.json
	cd $@* && python run.py --config ../data/config-ng${N}.json

# use marshmallow
04:
	# mypy --ignore-missing-imports --strict --strict-equality $@*/
	cd $@* && python dump.py | tee schema.yaml
	cd $@* && python run.py --config ../data/config.json
	cd $@* && python run.py --config ../data/config-ng${N}.json

setup:
	pip install -r requirements.txt --upgrade
