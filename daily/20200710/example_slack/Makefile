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

# for blog
x0:
	python $(shell echo $@*.py)
x1:
	python $(shell echo $@*.py)
x3:
	python $(shell echo $@*.py)
x4:
	python $(shell echo $@*.py)
x5:
	python $(shell echo $@*.py)

diff-x0x1:
	diff -u x0* x1*
diff-x0x3:
	diff -u x0* x3*
diff-x0x5:
	diff -u x0* x5*

token:
	echo $(SLACK_API_TOKEN)
