# setup:
# pip install httpie jqfpy

ME ?= podhmo

repos.json:
	http https://api.github.com/users/${ME}/repos page==1 per_page==100 | jq . | tee $@
repos2.json:
	http https://api.github.com/users/${ME}/repos page==2 per_page==100 | jq . | tee $@

parse: repos.json repos2.json
	jqfpy -c --squash 'from datetime import datetime as d; y = d(2017,1,1); [h.pick("name","created_at",d=x) for x in get() if not x["fork"] and d.strptime(x["created_at"], "%Y-%m-%dT%H:%M:%SZ") > y ]' repos*.json | jqfpy -c --squash --slurp 'sorted(get(), key=lambda x: x["created_at"])'
