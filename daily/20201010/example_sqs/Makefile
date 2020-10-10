NAME ?= queue-wrapper-sample.fifo
export AWS_REGION = ap-northeast-1

# send
00:
	go run $(shell echo $@*/)main.go -q $(NAME)
# recv
01:
	go run $(shell echo $@*/)main.go -q $(NAME)

# fake send/used with channel
02:
	go run $(shell echo $@*/)main.go -q $(NAME)
# fake send/used with cleanup
03:
	go run $(shell echo $@*/)main.go -q $(NAME)
# use real
04:
	go run $(shell echo $@*/)main.go -q $(NAME)
# use real
05:
	go run $(shell echo $@*/)main.go -q $(NAME)



get:
	aws sqs get-queue-url --queue-name=$(NAME) | jqfpy 'get("QueueUrl")' -r
list:
	aws sqs list-queues
create:
	aws sqs create-queue --queue-name $(NAME) --attributes "ContentBasedDeduplication=true,FifoQueue=true"
clean:
	aws sqs delete-queue --queue-url=$(shell make get -s)
.PHONY: get list create clean

# list-dead-letter-source-queues
