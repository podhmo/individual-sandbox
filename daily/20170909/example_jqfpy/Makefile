00:
	jqfpy 00data.yaml
	jqfpy -r '"{p[name]}({p[age]})".format(p=get("person"))' 00data.yaml
	jqfpy -o yaml 00data.yaml
	jqfpy -c -o yaml 00data.yaml
	cat 00data.yaml | jqfpy -r -i yaml 'get("person/name")'

01:
	jqfpy -o yaml 01data.json
	jqfpy -o yaml 01data.json | jqfpy -i yaml -c
