00:
	python $@*.py
01:
	# dot -Tsvg $@*.dot | tee $@.svg
	circo -Tsvg $@*.dot | tee $@.svg
	xdg-open $@.svg
02:
	python $@*.py
03:
	mkdir -p dst
	python $@*.py src/00*.py | tee dst/00.dot
	python $@*.py src/01*.py | tee dst/01.dot
