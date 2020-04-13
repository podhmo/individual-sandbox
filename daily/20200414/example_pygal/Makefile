SHELL := $(shell which bash)

# line
00:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.svg,$(shell echo $@*.py))
# bar
01:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.svg,$(shell echo $@*.py))
# histgram
02:
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.svg,$(shell echo $@*.py))
# scatter (with iris dataset)
03: iris.json
	python $(shell echo $@*.py) |& tee $(patsubst %.py,%.svg,$(shell echo $@*.py))

setup:
	pip install -r requirements.txt
iris.json:
	wget https://raw.githubusercontent.com/altair-viz/vega_datasets/master/vega_datasets/_data/iris.json
