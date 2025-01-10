00:
	TITLE=emotion-example TSX=emotion.tsx deno run -A ~/ghq/github.com/podhmo/deno-glue/main.ts serve --port 8080 --next app.ts
01:
	TITLE=emotion-example TSX=emotion.react18.tsx deno run -A ~/ghq/github.com/podhmo/deno-glue/main.ts serve --port 8080 --next app.ts
bundle:
	deno run -A ~/ghq/github.com/podhmo/deno-glue/main.ts bundle emotion.tsx --output-style html --next | tee emotion.html
clean:
	rm -f deno.lock
