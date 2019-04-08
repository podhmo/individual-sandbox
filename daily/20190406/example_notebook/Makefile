TARGET := ~/Downloads/"Archive 19-04-06 18-31-31.har"

00:
	dictknife shape ${TARGET} -i json --with-type > 00shape.txt
01:
	jqfpy 'get("log/entries[]/request/url")' ${TARGET} --squash --raw | sort > 01url.txt
02:
	jqfpy '[h.pick("request/url", "request/method", "response/status", d=d) for d in get("log/entries[]") if "static" not in get("request/url", d=d)]' ${TARGET} > 02requests.json
