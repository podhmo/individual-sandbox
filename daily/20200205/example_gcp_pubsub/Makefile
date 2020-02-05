PROJECT_ID := steady-voltage-181003
TOPIC_NAME := my-topic2
SUBSCRIPTION_NAME := my-sub2

export GOOGLE_APPLICATION_CREDENTIALS := $(HOME)/.config/cliauth/steady-voltage-181003.json

00:
	python $(shell echo $@*.py) --topic-name=$(TOPIC_NAME) --project-id $(PROJECT_ID)
01:
	python $(shell echo $@*.py) --topic=$(TOPIC_NAME) --project-id $(PROJECT_ID)
02:
	python $(shell echo $@*.py) --topic=$(TOPIC_NAME) --project-id $(PROJECT_ID) --sub=$(SUBSCRIPTION_NAME)

setup:
	gcloud pubsub topics create $(TOPIC_NAME)
	gcloud pubsub subscriptions create --topic $(TOPIC_NAME) $(SUBSCRIPTION_NAME)

clean:
	gcloud pubsub topics delete $(TOPIC_NAME)
	gcloud pubsub subscriptions delete $(SUBSCRIPTION_NAME)
