# sqs send batch
00:
	python $@*.py --queue-url $$(make -s get)
# sqs recv batch
01:
	python $@*.py --queue-url $$(make -s get)
# provider, consumer
02:
	LOGGING_LEVEL=INFO LOGGING_FORMAT="%(relativeCreated)d: %(message)s" python $@*.py --queue-url $$(make -s get)
# queue like
03:
	python $@*.py --queue-url $$(make -s get)

NAME ?= hello
setup:
	aws sqs create-queue --queue-name ${NAME}
clean:
	aws sqs delete-queue --queue-url $$(make -s get)
get:
	aws sqs get-queue-url --queue-name ${NAME} | jqfpy 'get("QueueUrl")' -r
