SLACKCLI_API_TOKEN ?= $(shell jqfpy 'get("slack/api-token")' ~/.config/monogusa/config.json -r )

00:
	@SLACKCLI_API_TOKEN=$(SLACKCLI_API_TOKEN) python $(shell echo $@*.py)
01:
	@SLACKCLI_API_TOKEN=$(SLACKCLI_API_TOKEN) python $(shell echo $@*.py)
02:
	@SLACKCLI_API_TOKEN=$(SLACKCLI_API_TOKEN) python $(shell echo $@*.py)
