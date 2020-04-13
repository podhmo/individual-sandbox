SHELL := bash
TARGET ?= src/stocks.yaml

# raw
00:
	time dictknife shape ${TARGET} | tee $@.shape

# shrink
01:
	time dictknife shrink ${TARGET} | tee $@shrink.yaml
	time dictknife shape $@shrink.yaml | tee $@.shape
# with tail
02:
	time dictknife shrink --with-tail ${TARGET} | tee $@shrink.yaml
	time dictknife shape $@shrink.yaml | tee $@.shape

10:
	swaggerknife json2swagger <(jqfpy 'get("0/response/content")' 02shrink.yaml)
