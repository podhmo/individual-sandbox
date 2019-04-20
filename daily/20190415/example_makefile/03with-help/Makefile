## Install npm dependencies for the api, admin, and frontend apps
install:
	@echo "Installing Node dependencies"
	@npm install

## Install dependencies and prepared development configuration
install-dev: install
	@./node_modules/.bin/selenium-standalone install
	@cp -n ./config/development.js-dist ./config/development.js | true

## Run the frontend and admin apps in dev (using webpack-dev-server)
run-frontend-dev: webpack.PID

webpack.PID:
	@./node_modules/.bin/babel-node ./node_modules/.bin/webpack-dev-server \
		--content-base=build \
		--devtool=cheap-module-inline-source-map \
		--hot \
		--inline \
		--progress \
		& echo "$$!" > webpack.PID

## Stop the frontend and admin apps in dev
stop-frontend-dev: webpack.PID
	@kill `cat $<` && rm $<
	@echo "Webpack server stopped"

## Restart the frontend and admin apps in dev
restart-frontend:
		@make stop-frontend-dev && make run-frontend-dev
		@echo "Frontend app restarted"

.PHONY: help

help:
	@cat $(MAKEFILE_LIST) | python -u -c 'import sys; import re; from itertools import tee,chain; rx = re.compile(r"^[a-zA-Z0-9\-_]+:"); xs, ys = tee(sys.stdin); xs = chain([""], xs); [print(f"""\x1b[36m{line.split(":", 1)[0]:20s}\x1b[0m\t{prev.lstrip("# ").rstrip() if prev.startswith("##") else "" }""") for prev, line in zip(xs, ys) if rx.search(line)]'

.DEFAULT_GOAL := help
