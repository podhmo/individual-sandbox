SHELL = bash

serve:
	esbuild --serve=8080 --servedir=. --outdir=dist --tsconfig=deno.json *.tsx
gen:
	esbuild --outdir=dist --tsconfig=deno.json *.tsx
