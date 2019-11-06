DB := ../00example/estore.sqlite

gen:
	python gen.py --db=sqlite:///${DB} | tee output.md

${DB}:
	$(MAKE) -C ../00example estore.sqlite
