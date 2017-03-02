URL = http://localhost:9200

scan-indices:
	http GET ${URL}/_cat/indices

scan-index:
	http GET ${URL}/value

create-index:
	# cat ./mapping0.json | http PUT ${URL}/value
	cat ./mapping.json | http PUT ${URL}/value

delete-index:
	http DELETE ${URL}/value

post-values:
	cat ./value1.json | http PUT ${URL}/value/value/1
	cat ./value2.json | http PUT ${URL}/value/value/2
	cat ./value3.json | http PUT ${URL}/value/value/3
	cat ./value4.json | http PUT ${URL}/value/value/4

get-value0:
	http GET '${URL}/value/value/_search?q=*:*'

get-value1:
	http GET '${URL}/value/value/_search?q=hello'
get-value2:
	http GET '${URL}/value/value/_search?q=HELLO'
get-value3:
	http GET '${URL}/value/value/_search?q=テスト'
get-value4:
	http GET '${URL}/value/value/_search?q=test'
get-value5:
	http GET '${URL}/value/value/_search?q=TEST'

get-value6:
	echo '{"query": {"match": {"text": "test"}}}' | http POST ${URL}/value/value/_search
get-value7:
	echo '{"query": {"match": {"text_mb": "test"}}}' | http POST ${URL}/value/value/_search
get-value8:
	echo '{"query": {"match": {"text_mb_icase": "test"}}}' | http POST ${URL}/value/value/_search

get-value9:
	echo '{"query": {"bool": {"should": [{"match": {"text": "test"}}]}}}' | http POST ${URL}/value/value/_search
get-value10:
	echo '{"query": {"bool": {"should": [{"match": {"text_mb": "test"}}]}}}' | http POST ${URL}/value/value/_search
get-value10:
	echo '{"query": {"bool": {"should": [{"match": {"text_mb_icase": "test"}}]}}}' | http POST ${URL}/value/value/_search
get-value12:
	echo '{"query": {"bool": {"should": [{"match": {"text": "test"}}, {"match": {"text_mb": "test"}}]}}}' | http POST ${URL}/value/value/_search
get-value13:
	echo '{"query": {"bool": {"should": [{"match": {"text": "test"}}, {"match": {"text_mb": "test"}}, {"match": {"text_mb_icase": "test"}}]}}}' | http POST ${URL}/value/value/_search
