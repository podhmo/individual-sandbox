AUTHOPT = -a elastic:changeme
URL = :19200
INDEXNAME = values

middleware:
	docker-compose up

clean:
	http ${AUTHOPT} DELETE ${URL}/${INDEXNAME}

create:
	cat index.json | cat 1>&2 | http --json ${AUTHOPT} PUT ${URL}/${INDEXNAME}
	# seq 100 | xargs -I{} echo '{"_id": {}, "name": "foo", "value": {}}' | jqfpy --slurp 'h.chunk(get(), n=25)' --squash -c | (while read LINE; do echo $$LINE | jqfpy '{"docs": get()}' | kamidana -i json transform.json.j2 | http ${AUTHOPT} -b POST ${URL}/_bulk 'Content-Type:application/x-ndjson'; done)
