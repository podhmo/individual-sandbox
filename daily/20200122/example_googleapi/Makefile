CONF ?= ~/.config/cliauth/google-client-secrets.json
export CLIENT_ID ?= $(shell jqfpy "get('installed/client_id')" $(CONF) -r)
export CLIENT_SECRET ?= $(shell jqfpy "get('installed/client_secret')" $(CONF) -r)

00:
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py)
