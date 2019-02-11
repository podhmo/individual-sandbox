default: 00 01 02 03

setup:
	pip install -r requirements.txt

dst:
	mkdir -p dst
00: dst
	python 00*.py --src src/with-hash.yaml | tee dst/00.yaml
01: dst
	python 01*.py --src src/with-hash.yaml | tee dst/01.yaml
02: dst
	python 02*.py --src src/with-hash.yaml | tee dst/02.yaml
03: dst
	python 03*.py --src src/with-hash.yaml | tee dst/03.yaml
04: dst
	python 04*.py --src src/with-hash.yaml | tee dst/04.yaml
