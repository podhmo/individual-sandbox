SHELL := $(shell which bash)
OUTPUT ?= articles.yaml

# show token
00:
	python $(shell echo $@*.py)
# get items
01:
	@python $(shell echo $@*.py) | tee $(OUTPUT)
# shape
02:
	dictknife shape $(OUTPUT) | tee $@.shape

# schema
03:
	swaggerknife json2swagger <(jqfpy -i yaml 'd = get(); [c.__setitem__("rendered_body", c["rendered_body"][:100]) or c.__setitem__("body", c["body"][:100]) for c in d["response"]["content"]]; d' $(OUTPUT)) | tee $@.schema.yaml

# api schema
04:
	http -b get https://qiita.com/api/v2/schema | jq .| tee schema.json

# stocks
05:
	python $(shell echo $@*.py) | tee stocks.yaml
