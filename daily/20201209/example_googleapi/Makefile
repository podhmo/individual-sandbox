FILE :=  ~/.config/sheetconf/credentials.json
FILE2 := ~/.config/sheetconf/authorized_user.json

00:
	python $(shell echo $@*.py) --credential-file=$(FILE) --token-file=$(FILE2)
01:
	python $(shell echo $@*.py) --credential-file=$(FILE) --token-file=$(FILE2)


client_id:
	jq -r .installed.client_id $(FILE)
.PHONY: client_id

client_secret:
	jq -r .installed.client_secret $(FILE)
.PHONY: client_secret

refresh_token:
	jq -r .refresh_token $(FILE2)
.PHONY: refresh_token

_schema_credentials.py:
	python -m metashape.inputs.python $(FILE) | tee $@
_schema_authorized_user.py:
	python -m metashape.inputs.python $(FILE2) | tee $@
