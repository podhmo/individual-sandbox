00:
	deno serve --port 8080 --allow-net $(shell echo $@*.ts)
01:
	deno serve --watch --port 8080 --allow-net $(shell echo $@*.ts)
02:
	deno serve --watch --port 8080 --allow-net $(shell echo $@*.tsx)
03:
	deno run -A ../../../../deno-bundle/main.ts 03components.tsx > 03components.js
	deno serve --watch --port 8080 -A $(shell echo $@*.ts)