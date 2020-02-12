00: API_TOKEN ?= $(shell jqfpy 'get("discord-api-token")' ~/.config/monogusa/config.json -r)
00:
	DISCORD_API_TOKEN=$(API_TOKEN) python $(shell echo $@*.py)
