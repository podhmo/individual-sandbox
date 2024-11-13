default: render3.html render2.html render1.html render0.html

render0.html: render0.tsx README.md
	deno run --allow-env --allow-read $< > $@
render1.html: render1.tsx README.md
	deno run --allow-env --allow-read $< --md README.md > $@
render2.html: render2.tsx
	deno run --allow-env --allow-read $< --md README.md > $@
render3.html: render3.tsx
	deno run --allow-env --allow-read $< --md README.md > $@