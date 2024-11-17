server: component.mjs
	python3 -m http.server 8080
.PHONY: server

component.mjs: component.tsx
	deno run -A ../example_deno2/tools2.ts --src component.tsx --dst component.mjs --config deno.json
