RUN ?= bin/streamlit --log_level=debug run

00:
	bin/streamlit --log_level=debug hello
01:
	$(RUN) $(shell echo $@*.py)

setup:
	pip install -r requirements.txt
