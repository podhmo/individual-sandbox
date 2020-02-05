NAME ?= hello

00:
	python $(shell echo $@*.py) run --queue-url $$(make -s get)
01:
	python $(shell echo $@*.py) --queue-url $$(make -s get)

setup:
#	pip install -r requirements.txt
	aws sqs create-queue --queue-name ${NAME}
purge:
	aws sqs purge-queue --queue-url $$(make -s get)
clean: purge
	aws sqs delete-queue --queue-url $$(make -s get)
get:
	aws sqs get-queue-url --queue-name ${NAME} | jqfpy 'get("QueueUrl")' -r
