00:
	ioknife too --cmd "make -s $@serve" --cmd "make -s $@request"
00serve:
	uvicorn $$(echo 00*.py | sed 's/\.py//'):app --port=8080
00request:
	sleep 1
	http GET http://localhost:8080/

01:
	ioknife too --cmd "make -s $@serve" --cmd "make -s $@request"
01serve:
	uvicorn $$(echo 01*.py | sed 's/\.py//'):app --port=8080 --reload
01request:
	sleep 1
	python -m webbrowser -t http://localhost:8080
