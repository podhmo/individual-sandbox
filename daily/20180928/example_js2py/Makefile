setup:
	pip install -r requirements.txt

00:
	python transpile.py 00hello.js | yapf | tee 00hello.py
	python 00hello.py

# github.com/podhmo/moduleknife
show-deps:
	module-graph 00hello.py
	dot -Tsvg 00hello.dot | tee 00hello.svg
