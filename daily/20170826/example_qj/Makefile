QJ = python qj.py

00:
	cat 00*.json | ${QJ} 'get()'
01:
	cat 01*.json | ${QJ} 'get()[0]'
02:
	cat 02*.json | ${QJ} 'max(get(), key=lambda x: x["a"])'
03:
	# https://github.com/stedolan/jq/wiki/Cookbook#filter-objects-based-on-the-contents-of-a-key
	cat 03*.json | ${QJ} '[d for d in get() if "house" in d["genre"]]'
	cat 03*.json | ${QJ} 'import re; rx = re.compile("house", re.I); [d for d in get() if rx.search(d["genre"])]'

jq: jq.tutorial.json
	# see: https://stedolan.github.io/jq/tutorial/

	# jq '.'
	cat $< | ${QJ} 'get()'

	# jq '.[0]'
	cat $< | ${QJ} 'get()[0]'

	# jq '.[0] | {message: .commit.message, name: .commit.committer.name}'
	cat $< | ${QJ} 'd = get()[0]; {"message": get("commit/message", d), "name": get("commit/committer/name", d)}'
	cat $< | ${QJ} '{"message": get("0/commit/message"), "name": get("0/commit/committer/name")}'

	# jq '.[] | {message: .commit.message, name: .commit.committer.name}'
	cat $< | ${QJ} --squash 'L = get(); [{"message": get("commit/message", d), "name": get("commit/committer/name", d)} for d in L]'

	# jq '[.[] | {message: .commit.message, name: .commit.committer.name}]'
	cat $< | ${QJ} 'L = get(); [{"message": get("commit/message", d), "name": get("commit/committer/name", d)} for d in L]'

	# jq '[.[] | {message: .commit.message, name: .commit.committer.name, parents: [.parents[].html_url]}]'
	cat $< | ${QJ} 'L = get(); [{"message": get("commit/message", d), "name": get("commit/committer/name", d), "parents": [p["html_url"] for p in d["parents"]]} for d in L]'

jq.tutorial.json:
	curl 'https://api.github.com/repos/stedolan/jq/commits?per_page=5' -o $@
