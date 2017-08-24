normalize:
	gsed -i "s:$$HOME:\$$HOME:g" *.txt

client:
	http :8080/gc; http :8080/gc; for i in `jot 10`; do http :8080/inc; done; http :8080/gc; http :8080/gc; http :8080/clear; http :8080/gc; http :8080/gc;
