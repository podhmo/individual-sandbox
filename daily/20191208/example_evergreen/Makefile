00:
	(cd $@* && ../node_modules/.bin/parcel serve index.html)
01:
	(cd $@* && ../node_modules/.bin/parcel serve index.html)


gen: NAME ?= sandbox
gen: N ?= $$(ls | grep -P "^\d+" | wc -l | xargs printf '%02d')
gen:
	cp -r 00* ${N}${NAME}
setup:
	npm install
browse:
	xdg-open http://localhost:1234
