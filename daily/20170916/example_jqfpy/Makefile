00:
	cat 00data.json | jqfpy --squash -r 'get("hits/hits[]/_source/user")'

01:
	jqfpy 'h.pick("group/members[]/name")' 01data.yaml
	jqfpy 'h.pick("group/members[]/name@names")' 01data.yaml

02:
	cat 00data.json | jqfpy 'get("hits/hits/0/_source/user")'

03:
	cat 00data.json | jqfpy --squash 'L = get("hits/hits"); [get("_source/user", d=d) for d in L]'

04:
	cat 00data.json | jqfpy --squash 'get("hits/hits[]/_source/user")'

05:
	cat 00data.json | jqfpy 'get("hits/hits/0/*/message")'

06:
	cat 00data.json | jqfpy --squash -r 'get("hits/hits[]/*/message")'

07:
	cat 02data.json | jqfpy 'get("apps/*/use")'

08:
	cat 02data.json | jqfpy --squash 'get("apps/*[]/use")'

09:
	cat 02data.json | jqfpy --squash '["{k}:{v[use]}".format(k=k,v=v) for k, v in get("apps").items() if "use" in v]'

10:
	cat 02data.json | jqfpy --squash -o ltsv '[{"name": k, "use": v["use"]} for k, v in get("apps").items() if "use" in v]'

11:
	cat 02data.json | jqfpy --squash -o ltsv 'h.pick("apps/name")'
