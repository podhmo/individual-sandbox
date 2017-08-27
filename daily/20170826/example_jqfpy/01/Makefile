jq:
	bash -c 'python gen.py | tail -f | while read LINE; do echo "$${LINE}" | jq --unbuffered ".n" ; done'

jqfpy:
	bash -c 'python gen.py | tail -f | while read LINE; do echo "$${LINE}" | python -m jqfpy "get(\"n\")" ; done'

jqfpy2:
	bash -c 'python gen.py | tail -f | python -m jqfpy --squash -s "[get(\"n\", x) for x in get()]"'
