default: 00 01

00: swagger.json
	cat swagger.json | jqfpy --squash -r 'import dictknife.walkers as w; itr = w.DictWalker(["$$ref"]).iterate(get()); [d["$$ref"] for _, d in itr]'

01: swagger.json
	cat swagger.json | jqfpy 'from collections import defaultdict; import dictknife.walkers as w; D = defaultdict(list); itr = w.DictWalker(["$$ref"]).iterate(get()); [D[d["$$ref"]].append("/".join([p.replace("/", "~1") for p in path[:-1]])) for path, d in itr]; D'

swagger.json:
	curl https://raw.githubusercontent.com/everett-toews/a-restful-adventure/gh-pages/design/hypermedia-based/swagger.json > $@
