# OK: object,primitive
00:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# NG: array,object
01:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# OK: object,array,primitive
02:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# OK: object,array,object
03:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# NG: object,array,array,primitive
04:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# NG: object,dict,primitive
05:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# NG: object,dict,object
06:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# NG: object,additionals
07:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# OK: object,object
08:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# OK: object,self
09:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# NG primitive
10:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# NG array,primitive
11:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# v3
12:
	python -m swagger_marshmallow_codegen $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
