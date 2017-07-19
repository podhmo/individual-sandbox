readme:
	kamidana -a additionals.py readme.md.j2 > readme.md

hello:
	http :19200

indices:
	http :19200/_cat/indices

create-index:
	cat mappings/site-entry-search.json | http PUT :19200/site-entry-search

put-data:
	for i in `jot 3 1`; do cat data/$$i.json | http PUT :19200/site-entry-search/site-search/$$i; done

search:
	for i in `jot 4 1`; do cat queries/$$i.json; cat queries/$$i.json | http POST :19200/site-entry-search/site-search/_search pretty==1 | tee outputs/$$i.json; done

delete:
	http DELETE :19200/site-entry-search

step: delete create-index put-data search
