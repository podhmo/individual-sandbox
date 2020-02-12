NAME = foo
CONT ?= cat

create:
	python -m minitask.worker.sqsworker create --fifo $(NAME) | $(CONT)
list:
	python -m minitask.worker.sqsworker list | $(CONT)
delete:
	python -m minitask.worker.sqsworker delete $(NAME) | $(CONT)
