NAME ?= hello

00:
	python $(shell echo $@*.py) --queue-url $$(make -s get)
01:
	python $(shell echo $@*.py) --queue-url $$(make -s get)

setup:
	aws sqs create-queue --queue-name ${NAME}
clean:
	aws sqs delete-queue --queue-url $$(make -s get)
get:
	aws sqs get-queue-url --queue-name ${NAME} | jqfpy 'get("QueueUrl")' -r
