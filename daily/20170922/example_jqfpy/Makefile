00:
	jqfpy --slurp "L = get(); list(set(L[0]).difference(L[1]))" 00xs.json 00ys.json
	jqfpy --slurp "L = get(); list(set(L[0]).difference(L[1]))" 00ys.json 00xs.json

01:
	echo '[1,2,3,4,5]' > xs.json
	echo '[2,4,6,8,10]' > ys.json
	# x * y
	jqfpy --slurp 'list(set(get("0")).intersection(get("1")))' xs.json ys.json
	# x + y
	jqfpy --slurp 'list(set(get("0")).union(get("1")))' xs.json ys.json
	# x - y
	jqfpy --slurp 'list(set(get("0")).difference(get("1")))' xs.json ys.json
	# y - x
	jqfpy --slurp 'list(set(get("0")).difference(get("1")))' ys.json xs.json
