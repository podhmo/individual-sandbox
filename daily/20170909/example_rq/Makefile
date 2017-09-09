setup:
	pip install -r requirements.txt

env:
	docker-compose up

worker:
	(cd src; rq worker)
