00: API_TOKEN ?= $(shell jqfpy 'get("slack-api-token")' ~/.config/monogusa/config.json -r)
00:
	SLACKBOT_API_TOKEN=$(API_TOKEN) python $(shell echo $@*.py)
