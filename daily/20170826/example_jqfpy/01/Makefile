jq:
	bash -c 'python gen.py | tail -f | while read LINE; do echo "$${LINE}" | jq --unbuffered ".n" ; done'
jq2:
	bash -c 'python gen.py jq --unbuffered ".n"'

jqfpy:
	bash -c 'python gen.py | tail -f | while read LINE; do echo "$${LINE}" | python -m jqfpy "get(\"n\")" ; done'
