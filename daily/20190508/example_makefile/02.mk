taskA: VALUE ?= world
taskA:
	@echo hello ${VALUE}

taskB: VALUE ?= you
taskB:
	@echo kill ${VALUE}
taskC: password := $(shell jq .db.password -r config.json)
taskC:
	echo script -p "${password}"
