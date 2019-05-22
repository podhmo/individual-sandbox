parse: stats.json
	jqfpy --slurp 'data = [{"package": d["package"], **d["data"]} for d in get()]; sorted(data, key=lambda d: d["last_month"] + 4 * d["last_week"], reverse=True)' stats.json | dictknife cat -i json -o md | tee sorted.md

add:
	pypistats recent flake8 -f json >> stats.json
	pypistats recent pandas -f json >> stats.json
	pypistats recent flask -f json >> stats.json
	pypistats recent arrow -f json >> stats.json
	pypistats recent twine -f json >> stats.json
	pypistats recent marshmallow -f json >> stats.json
	pypistats recent aiohttp -f json >> stats.json

stats.json:
	pip search pytest | grep -v '^ ' | cut -d " " -f 1 | xargs -I{} pypistats recent {} -f json | tee stats.json
