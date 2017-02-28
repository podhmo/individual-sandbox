server:
	python 00server.py --port=4444

client:
	http :4444/value
	http :4444/value offset==10
	http :4444/value offset==10 limit==20
	http :4444/value offset==100 limit==10
	http :4444/value offset==99 limit==10
	http :4444/value offset==101 limit==10
