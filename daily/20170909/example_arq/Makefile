setup:
	pip install -r requirements.txt

env:
	docker-compose up

worker:
	arq src/00*.py

00:
	arq --check src/00*.py
	python src/00*.py
