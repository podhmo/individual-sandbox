export SLACK_API_TOKEN ?= $(shell python -m jqfpy -r 'get("slack/api-token")' ~/.config/monogusa/config.json)
export DEBUG ?= 1
00:
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py)
02:
	python $(shell echo $@*.py)
	# python -m monogusa.cli $(shell echo $@*.py) run
03:
	python $(shell echo $@*.py)

token:
	echo $(SLACK_API_TOKEN)
