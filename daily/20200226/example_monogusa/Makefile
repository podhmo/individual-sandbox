export SLACKCLI_API_TOKEN = $(shell jqfpy 'get("slack/api-token")' ~/.config/monogusa/config.json -r)

00:
	python -m monogusa.chatbot.slackcli $(shell echo $@*.py)
01:
	python -m monogusa.chatbot.slackcli $(shell echo $@*.py)
02:
	python $(shell echo $@*.py) sample
03:
	python $(shell echo $@*.py)
04:
	python $(shell echo $@*.py) sample
05:
	python $(shell echo $@*.py) sample

sqs:
	# python -m minitask.worker.sqsworker create foo
	# aws sqs create-queue --queue-name minitask_foo
	python -m minitask.worker.sqsworker list
