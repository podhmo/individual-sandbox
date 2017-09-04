default: 00

00:
	jqfpy -r --squash 'import re; rx = re.compile("data"); [x["Id"] for x in get() if not any(rx.search(name) for name in x["Names"])]' data.json

01:
	jqfpy -r --squash '[x["Id"] for x in get() if not any("data" in name for name in x["Names"])]' data.json

02:
	jq -r 'map(select(any(.Names[]; contains("data"))|not)|.Id)[]' data.json
