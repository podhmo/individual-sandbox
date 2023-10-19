SHELL := bash
ESBUILD := $(shell ls ./node_modules/.bin/esbuild 2>/dev/null || echo esbuild)

# just *.jsx -> js
00:
	(cd $(shell echo $@*/); $(ESBUILD) app.jsx --outfile=out.js)

# with source map
01:
	(cd $(shell echo $@*/); $(ESBUILD) app.jsx --sourcemap=external --outfile=out.js)
