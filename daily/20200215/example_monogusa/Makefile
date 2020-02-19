00:
	@SLACKCLI_API_TOKEN=$(shell jqfpy 'get("slack-api-token")' ~/.config/monogusa/config.json -r ) python -m monogusa.chatbot.slackcli $(shell echo $@*.py)
01:
	@SLACBOT_API_TOKEN=$(shell jqfpy 'get("slack-api-token")' ~/.config/monogusa/config.json -r ) python $(shell echo $@*.py)
