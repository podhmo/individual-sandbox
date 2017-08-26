jqfpy:
	python gen.py | jqfpy --slurp 'get("n")' | tee output.txt

jq:
	python gen.py | jq --slurp '.[] | .n' | tee output.txt
