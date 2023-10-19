SHELL := bash
ESBUILD := $(shell ls ./node_modules/.bin/esbuild 2>/dev/null || echo esbuild)

# just *.jsx -> js
00:
	(cd $(shell echo $@*/); $(ESBUILD) app.jsx --outfile=out.js || exit 1)

# with source map
01:
	(cd $(shell echo $@*/); $(ESBUILD) app.jsx --sourcemap=external --outfile=out.js || exit 1)

# tsconfig.json (preact)
02:
	(cd $(shell echo $@*/); $(ESBUILD) app.jsx --outfile=out.js || exit 1)
# html
03:
	(cd $(shell echo $@*/); $(ESBUILD) app.jsx --outfile=out.js || exit 1)
03-server: 03
	python -m http.server --directory $@*/ 8000

clean:
	rm -rf */out.js */out.js.map
