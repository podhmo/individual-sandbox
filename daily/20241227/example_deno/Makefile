00: 00.html
00.html: 00client.tsx
	deno run -A jsr:@podhmo/glue/bundle --output-style html --html-id app $< > $@

01: 01.html
01.html: 01client.tsx
	deno run -A jsr:@podhmo/glue/bundle --output-style html --html-id app $< > $@

02:
	deno serve --port 8080 -A 02app.ts
03:
	deno serve --port 8080 -A 03app.tsx
