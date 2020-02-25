DISCORDCLI_API_TOKEN ?= $(shell jqfpy 'get("discord/api-token")' ~/.config/monogusa/config.json -r )
export DEBUG ?= 
00:
	@DISCORDCLI_API_TOKEN=$(DISCORDCLI_API_TOKEN) python $(shell echo $@*.py)
01:
	@DISCORDCLI_API_TOKEN=$(DISCORDCLI_API_TOKEN) python $(shell echo $@*.py)
