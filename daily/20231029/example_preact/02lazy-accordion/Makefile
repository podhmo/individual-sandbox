SHELL = bash

serve:
	esbuild --serve=8080 --servedir=. --outfile=app.js app.tsx

main.js: app.tsx main.ts
	esbuild --bundle --outfile=main.js --keep-names main.ts
