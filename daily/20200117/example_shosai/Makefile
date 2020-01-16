# in shell, export ARTICLE_URL=xxx
ARTICLE_URL ?= "https://<area>.docbase.io/posts/<post id>"

00:
	echo $(ARTICLE_URL)

# setup .config/shosai
01:
	shosai docbase clone -h
02:
	python -m reqtrace shosai docbase clone $(ARTICLE_URL)
03:
	shosai docbase search -h
04:
	python -m reqtrace shosai docbase search -q "go"
05:
	shosai docbase tags -h
06:
	shosai docbase tags
07:
	shosai docbase groups -h
08:
	shosai docbase groups
09:
	shosai docbase push -h
10: ARTICLE := ./docs/hello.md
10:
	shosai docbase push ${ARTICLE}
