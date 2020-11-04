GEN := python -m swagger_marshmallow_codegen --strict-additional-properties

# OK: object,primitive
00:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# NG: array,object
01:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# OK: object,array,primitive
02:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# OK: object,array,object
03:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# NG: object,array,array,primitive
04:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# 直したい
# NG: object,dict,primitive
05:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# NG: object,dict,object
06:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# NG: object,additionals
07:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# OK: object,object
08:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# OK: object,self
09:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
# NG primitive
10:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# NG array,primitive
11:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))

# v3
12:
	$(GEN) $(shell echo $@*.yaml) | tee $(patsubst %.yaml,%.py,$(shell echo $@*.yaml))
