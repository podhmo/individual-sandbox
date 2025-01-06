serve:
	rm -f deno.lock
	echo {} > deno.json
	deno cache *.tsx
	deno run -A jsr:@podhmo/glue@0.2.3 serve --port 8080 ./app.ts --debug
.PHONY: serve

bundle:
	deno run -A jsr:@podhmo/glue@0.2.3 bundle ./client.tsx --output-style html --html-id app > index.html
.PHONY: bundle