00:
	python 00*.py --src data/2.0.yaml --dst data/3.0.yaml | tee 00.output
01:
	python 01*.py --src data/2.0.yaml --dst data/3.0.yaml | tee 01.output
02:
	python 02*.py --src data/2.0.yaml --dst data/3.0.yaml | tee 02.output
diff: 00 01
	dictknife diff -i json -S --normalize 00.output 01.output | tee /tmp/a.diff
