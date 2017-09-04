default: 00

00:
	jqfpy --slurp 'L = get(); from dictknife import deepmerge; d = deepmerge(*L); {"vaulue": d["value"]}'  00data.json 01data.json

01:
	jq -s '.[0] * .[1] | {value: .value}' 00data.json 01data.json
