NAME ?= hello
CONFIG_PATH := queue-${NAME}.json

00:
	aws sqs list-queues
01:
	aws sqs get-queue-url --queue-name ${NAME} | tee queue-${NAME}.json
02:
	aws sqs get-queue-url --queue-name ${NAME} | jqfpy 'get("QueueUrl")' -r
03:
	$(info NAME = ${NAME})
	$(info CONFIG_PATH = ${CONFIG_PATH})
	python $@*.py --config-path=${CONFIG_PATH}

# send message
04:
	python $@*.py --config-path=${CONFIG_PATH}

05:
	aws sqs receive-message --queue-url $$(make -s 02)
# receive message
06:
	python $@*.py --config-path=${CONFIG_PATH}
help:
	aws sqs help
create:
	aws sqs create-queue --queue-name ${NAME}
clean:
	echo aws sqs delete-queue --queue-url $$(make -s 02)
