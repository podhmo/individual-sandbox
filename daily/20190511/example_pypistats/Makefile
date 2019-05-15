GOOGLE_CREDENTIAL_FILE="<>.json"

stats: PKG ?= flask
stats: CMD ?= python_minor
stats:
	pypistats ${CMD} ${PKG} --last-month -f markdown

overall: PKG ?= flask
overall: FORMAT ?= markdown
overall:
	@pypistats overall -m 2019-01 ${PKG} -f ${FORMAT}
	@pypistats overall -m 2019-02 ${PKG} -f ${FORMAT}
	@pypistats overall -m 2019-03 ${PKG} -f ${FORMAT}
	@pypistats overall -m 2019-04 ${PKG} -f ${FORMAT}
	@pypistats overall -m 2019-05 ${PKG} -f ${FORMAT}


# examples
examples: PKG ?= flask
examples: START ?= 2019-04-01
examples: END ?= 2019-05-01
examples:
	# python version
	pypistats python_minor ${PKG} --start ${START} --end ${END} -f markdown
	pypinfo --start-date ${START} --end-date ${END} --percent --markdown ${PKG} pyversion
	# system
	pypistats system ${PKG} --start ${START} --end ${END} -f markdown
	pypinfo --start-date ${START} --end-date ${END} --percent --markdown ${PKG} pyversion

examples2:
	pypistats python_major ${PKG} --last-month
	pypistats python_minor ${PKG} -f json --last-month |  jq .
#	pypinfo --auth=${GOOGLE_CREDENTIAL_FILE}
	pypinfo --start-date 2017-06-01 --end-date 2018-05-31 --percent --markdown pymagicc pyversion

setup:
	pip install -r requirements.txt

diff:
	pypistats python_minor --month 2019-04 -f markdown flask > flask.md
	pypistats python_minor --month 2019-04 -f markdown django > django.md
	paste flask.md django.md | sed 's/|[ \t]|/|/g' > diff.md

diff2:
	pypistats python_minor --month 2019-04 -f markdown autopep8 > autopep8.md
	pypistats python_minor --month 2019-04 -f markdown yapf > yapf.md
	pypistats python_minor --month 2019-04 -f markdown black > black.md
	paste autopep8.md yapf.md black.md | sed 's/|[ \t]|/|/g' > diff2.md

diff3:
	pypistats python_minor --month 2019-04 -f json flask > flask.json
	pypistats python_minor --month 2019-04 -f json django > django.json
	# pip install jqfpy
	jqfpy --squash '[row["category"] for row in get("data")]' flask.json django.json | sort -u > headers.json
	jqfpy '{row["category"]: row["value"] for row in get("data")}' flask.json
