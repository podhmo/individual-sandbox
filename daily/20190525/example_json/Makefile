default: jq jqfpy

jqfpy:
	jqfpy people.json --squash -c | tee jqfpy.output
jq:
	jq -c '.[]' people.json  | tee jq.output
