00:
	pip freeze | grep foo
01:
	python $(shell echo $@*.py)
show-structure:
	tree -I "__pycache__" -P "*.py" .
setup:
	pip install -r requirements.txt

clean:
	pip uninstall foo-bar foo-boo
