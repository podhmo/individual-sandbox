setup:
	pip install -r requirements.txt

00:
	python transpile.py 00hello.js | yapf | tee 00hello.py
	python 00hello.py

# github.com/podhmo/moduleknife
show-deps:
	modulegraph 00hello.py | grep -v "^'Hello" > 00hello.dot
	dot -Tsvg 00hello.dot | tee 00hello.svg

# semver
semver:
	test -d node-semver || git clone --depth=1 https://github.com/isaacs/node-semver
	python transpile.py node-semver/semver.js | yapf | tee semver.py
