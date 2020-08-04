URL ?= :19200
INDEX ?= my_index
OPTS ?= pretty==true error_trace==true

# insert index
00:
	cat $@req.json | http --json PUT $(URL)/$(INDEX) $(OPTS)
# bulk insert
01:
	cat $@req.json | http --json PUT $(URL)/_bulk $(OPTS)
# query all
02:
	http $(URL)/$(INDEX)/_search $(OPTS) | tee $@.output

# https://www.elastic.co/guide/en/elasticsearch/reference/7.x/query-dsl-script-score-query.html#vector-functions
03:
	cat $@req.json | http --json POST $(URL)/$(INDEX)/_search $(OPTS) | tee $@.output
# https://www.elastic.co/guide/en/elasticsearch/reference/current/search-request-body.html#request-body-search-script-fields
04:
	cat $@req.json | http --json POST $(URL)/$(INDEX)/_search $(OPTS) | tee $@.output

env:
	docker-compose up

# # insert data
# 00:
# 	00req.json
