publisher: INPUT ?= 7
publisher:
	KALE_SETTINGS_MODULE=taskworker.settings python publisher.py -n ${INPUT}

worker:
	KALE_SETTINGS_MODULE=taskworker.settings python worker.py

env:
	docker-compose up
