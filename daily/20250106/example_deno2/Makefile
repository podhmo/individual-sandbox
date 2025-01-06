sync:
	deno cache *.tsx
.PHONY: sync

00: sync
	TSX=$@client.tsx deno run -A jsr:@podhmo/glue@0.2.3 serve --port 8080 app.ts
.PHONY: 00

01: sync
	TSX=$@client.tsx deno run -A jsr:@podhmo/glue@0.2.3 serve --port 8080 app.ts --debug
.PHONY: 01

bundle:
	deno run -A jsr:@podhmo/glue@0.2.3 bundle --output-style html --html-id app 01client.tsx > 01.html