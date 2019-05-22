parse: django.json flask.json
	echo '{"left": "django.json", "right": "flask.json"}' | jqfpy 'd = get(); h.merge(h.loadfile(d["left"])["data"], h.loadfile(d["right"])["data"], how="outer", on="category")' --additionals merge.py

stats.json: django.json flask.json
	cat $^ | tee $@

django.json:
	pypistats python_minor django -f json | tee $@
flask.json:
	pypistats python_minor flask -f json | tee $@
