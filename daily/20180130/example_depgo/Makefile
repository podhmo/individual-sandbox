GOROOT = /opt/local/lib/go
# go tool dist env | grep GOROOT

clean:
	rm -rf parsed

parse:
	mkdir -p parsed
	for i in `find ${GOROOT}/src/go -type d -mindepth 1`; do python 00parse.py $$i | sort -u | tee parsed/`echo $$i | sed 's@${GOROOT}/src/@@g; s@/@_@g;'`.txt; done

graph:
	python 01graph.py parsed | tee graph.dot
	dot -Tsvg graph.dot > graph.svg
.NOPHONY: parse graph
