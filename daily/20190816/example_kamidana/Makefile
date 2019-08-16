run:
	docker-compose run --rm python kamidana ng.j2
01:
	python 01*.py ng.j2
02:
	docker-compose run --rm python python 01*.py ng.j2
03:
	docker-compose run --rm python python 02*.py ng.j2
04:
	docker-compose run --rm python python 03*.py ng.j2
05:
	kamidana undefined.j2
06:
	docker-compose run --rm python python 03* undefined.j2
07:
	 python 03* undefined.j2
