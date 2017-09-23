AUTHOPT = -a elastic:changeme
URL = :19200
INDEXNAME = values

default:
	$(MAKE) clean
	$(MAKE) create
	$(MAKE) check
	$(MAKE) search

middleware:
	docker-compose up

clean:
	http ${AUTHOPT} DELETE ${URL}/${INDEXNAME}

check:
	http ${AUTHOPT} ${URL}/
	http -b --json ${AUTHOPT} "${URL}/_cat/indices"

create:
	cat index.json | http --json ${AUTHOPT} PUT ${URL}/${INDEXNAME}
	bash gen.sh 100 | jqfpy --slurp 'h.chunk(get(), n=25)' --squash -c | (while read LINE; do echo $$LINE | bash transform.sh | http ${AUTHOPT} -b POST ${URL}/_bulk 'Content-Type:application/x-ndjson'; done)

search:
	http -b --json ${AUTHOPT} "${URL}/${INDEXNAME}/_search?q=*:*&pretty"
	http -b --json ${AUTHOPT} "${URL}/${INDEXNAME}/_count?q=*:*&pretty"
