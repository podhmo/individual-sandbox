DB:
	$(MAKE) -C ../../20191104/example_pony/00example estore.sqlite

00: DB
	rm -r $@*/doc
	cd $@* && tbls doc
01: DB
	python $@*.py

# cd 00*/ && tbls out
# todo:
02:
	# cd $@* && go run main.go ./schema
	python -m metashape.outputs.openapi --aggressive $@*/shape.py
