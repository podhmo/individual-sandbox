default: 00 01 02 03 04

00:
	# indent
	jqfpy 00data.json

01:
	# [0]
	jqfpy 'get("0")' 00data.json

02:
	# age > 30
	jqfpy '[v for v in get() if int(v["age"]) > 30]' 00data.json

03:
	# age > 30 only name
	jqfpy --squash '[v["name"] for v in get() if int(v["age"]) > 30]' 00data.json
04:
	jqfpy 'len(get())' 00data.json
