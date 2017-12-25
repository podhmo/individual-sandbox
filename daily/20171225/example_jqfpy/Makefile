00:
	jqfpy '[d["name"] for d in get()]' data.json --squash -r
01:
	jqfpy '[get("skills[]/name", d=d) for d in get()]' data.json --squash -c
02:
	jqfpy 'get("[]/name")' data.json --squash -r
03:
	jqfpy 'get("[]/skills[]/name")' data.json --squash -c
