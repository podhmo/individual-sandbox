default:
	find . -name "__pycache__" | xargs rm -rf
	python -m zipapp . -m main:main -o main.pyz

deps:
	for i in `python -c 'import starlette as m; [print(line) for line in m.__spec__.submodule_search_locations]'`; do cp -r $$i .; done
