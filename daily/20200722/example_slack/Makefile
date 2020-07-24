export SLACK_API_TOKEN := $(shell cat  ~/.config/monogusa/config.json | jqfpy 'get("slack/api-token")' -r)
export SLACK_DEFAULT_CHANNEL := \#stream_nao

PY_FILE = $(shell echo $@*.py)

# post message
00:
	python $(PY_FILE)
01:
	python $(PY_FILE)
02:
	python $(PY_FILE)
03:
	python $(PY_FILE)
04:
	python $(PY_FILE)

# update messages
05:
	python $(PY_FILE)
06:
	python $(PY_FILE)

# reaction
07:
	python $(PY_FILE)

# block?
08:
	python $(PY_FILE)

# communication
09:
	python $(PY_FILE)
10:
	DEBUG=1 python $(PY_FILE)
11:
	DEBUG=1 python $(PY_FILE) --auth-file=".auth.out"
12: clean
	DEBUG=1 python $(PY_FILE) --auth-file=".auth.out"
13: clean
	DEBUG=1 python $(PY_FILE) bot --auth-file=".auth.out"

clean:
	rm -f .auth.out
