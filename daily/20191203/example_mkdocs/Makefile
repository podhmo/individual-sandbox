serve:
	mkdocs serve

setup: bin/pip
	bin/pip install -r requirements.txt
bin/pip:
	python -m venv .

# using docker from https://squidfunk.github.io/mkdocs-material/getting-started/#alternative-using-docker
# docker pull squidfunk/mkdocs-material
# docker run --rm -it -p 8000:8000 -v ${PWD}:/docs squidfunk/mkdocs-material
