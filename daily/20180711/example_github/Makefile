TARGETNAME ?= jupyter
URL := https://api.github.com/users/${TARGETNAME}/repos

default:
	$(MAKE) parse
	$(MAKE) parse TARGETNAME=podhmo

fetch: $(addsuffix .json,$(addprefix data/,${TARGETNAME}))
parse: $(addsuffix .parsed.json,$(addprefix data/,${TARGETNAME}))

data/${TARGETNAME}.json:
	mkdir -p data
	http -b --pretty=format ${URL} sort==updated direct==desc per_page==100 | tee $@

data/${TARGETNAME}.parsed.json: data/${TARGETNAME}.json
	jqfpy '[h.pick("ssh_url", "stargazers_count", "updated_at", d=d) for d in get()]' $^ | tee $@

clean:
	rm -rf data/*
