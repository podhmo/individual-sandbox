00:
	deno run --allow-net $(shell echo $@*.ts) --port 8080
01:
	deno serve --allow-net --host 127.0.0.1 --port 8080 $(shell echo $@*.ts)
02:
	deno serve --allow-net --host 127.0.0.1 --port 8080 $(shell echo $@*.ts)
03:
	deno serve --watch-hmr=$(shell echo $@*.ts) --allow-net --host 127.0.0.1 --port 8080 $(shell echo $@*.ts)
04:
	deno run --allow-env $(shell echo $@*.tsx)
05:
	deno serve --watch-hmr=$(shell echo $@*.tsx) --allow-env=NODE_ENV --allow-net --host 127.0.0.1 --port 8080 $(shell echo $@*.tsx)
