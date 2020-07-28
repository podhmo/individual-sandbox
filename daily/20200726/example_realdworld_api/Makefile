SHELL := $(shell which bash)

# list components
00: swagger.json
	cat swagger.json | jqfpy '[name for name in get("definitions").keys()]' --squash -r

# list components from marshmallow
01: marshmallow_schema.py
	grep "^class .*" $< | cut -d " " -f 2 | cut -d "(" -f 1

# diff
02:
	diff -u <($(MAKE) 00 -s | sort) <($(MAKE) 01 -s | sort) 

# metashape
03: objects.py
	python -m metashape.outputs.openapi --aggressive $<

# parse services
FORMAT ?= | black -
04: swagger.json
	python _parse.py $< $(FORMAT) | tee services.py

marshmallow_schema.py: swagger.json
	swagger-marshmallow-codegen swagger.json | tee $@
# marshmallow_schema_full.py: swagger.json
# 	swagger-marshmallow-codegen --full swagger.json | tee $@
swagger.json:
	rm -f $@ # for make -B
	wget https://raw.githubusercontent.com/gothinkster/realworld/master/api/swagger.json
