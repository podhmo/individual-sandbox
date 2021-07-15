PYTHON ?= venv/bin/python
STREAMLIT ?= venv/bin/streamlit

run: $(STREAMLIT)
	$(STREAMLIT) run --server.port 8501 --server.headless true app.py

# build:
# 	docker build -t xxx:master .
# docker-run:
# 	docker run -p 44444:44444 --rm -it xxx:master streamlit run --server.port 44444 app.py
#	# open http://localhost:44444

venv/bin/streamlit: venv/bin/python
venv/bin/python:
	python -m venv venv
	$@ -m pip install -r requirements.txt
