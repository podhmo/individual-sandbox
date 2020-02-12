NAME ?= foo.fifo

00:
	python $(shell echo $@*.py) run --queue-url $(shell make get -s)
01:
	python $(shell echo $@*.py) sender --queue-url https://ap-northeast-1.queue.amazonaws.com/784330574880/foo.fifo

get:
	aws sqs get-queue-url --queue-name=$(NAME) | jqfpy 'get("QueueUrl")' -r

list:
	aws sqs list-queues

create:
	aws sqs create-queue --queue-name $(NAME) --attributes "ContentBasedDeduplication=true,FifoQueue=true"

clean:
	aws sqs delete-queue --queue-url=$(shell make get -s)

# list-dead-letter-source-queues
