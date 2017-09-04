00:
	cat 00data.json | jqfpy 'get("name")'

01:
	jqfpy -c 'get()' 01data.json
	jqfpy --slurp 'sorted(get(), key=lambda x: int(x["age"]))' 01data.json
	jqfpy --squash --slurp 'sorted(get(), key=lambda x: int(x["age"]))' 01data.json
	jqfpy --squash --slurp 'L = [x for x in get() if int(x["age"]) % 2 == 0]; sorted(L, key=lambda x: int(x["age"]))' 01data.json
	jqfpy --squash --slurp -c 'import re; rx = re.compile("o{2}"); [x for x in get() if rx.search(x["name"])]' 01data.json

02:
	jqfpy --slurp 'L = get(); maxv = int(L[-1]["value"]); minv = int(L[0]["value"]); min([n for n, x in zip(range(minv, maxv), L) if n != int(x["value"])] + [maxv + 1])' 02data.json
