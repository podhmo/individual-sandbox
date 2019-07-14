default: 01 02 03 04

01:
	$(MAKE) gen pkg=marshmallow
02:
	$(MAKE) gen pkg=starlette
03:
	$(MAKE) gen pkg=uvicorn
04:
	$(MAKE) gen pkg=fastapi

gen: pkg ?= collections
gen:
	python 01*.py ${pkg} | tee ${pkg}.yaml
	python 02*.py ${pkg}.yaml | tee ${pkg}.tsv
