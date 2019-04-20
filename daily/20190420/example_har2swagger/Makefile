defaut: 00 01 02 03 04 05 06 07

output:
	mkdir -p output

00: FILE ?= ../..//20190208/example_har/data.har
00: output
	python parse.py --pattern hatena --file=${FILE} --format=yaml --with-request-type --with-response-type | tee output/00.swagger.md

01: FILE ?= $(HOME)/Downloads/note.har
01: output
	python parse.py --pattern note --file=${FILE} --with-request-type --with-response-type | tee output/01.swagger.md

02: FILE ?= data.json
02: output
	python makeschema.py --file=${FILE} | tee output/02.swagger.md

03: data/sample.har output
	python parse.py --pattern . --include-all --with-request-type --with-response-type --file=$< | tee output/03.swagger.md
data/sample.har:
	mkdir -p data
	wget https://gist.githubusercontent.com/ericduran/6330201/raw/a34cd62995b3af7b95755bde56f0f80e7d8a44bc/ericduran.github.io.har -O data/sample.har

04: FILE ?= $(HOME)/Downloads/hatena.har
04: output
	python parse.py --pattern hatena --ignore cdn --with-request-type --with-response-type --file=${FILE} | tee output/04.swagger.md

05: FILE ?= $(HOME)/Downloads/hatena.har
05: output
	python parse.py --pattern hatena --ignore cdn --with-request-type --with-response-type --shy --file=${FILE} | tee output/05.swagger.md


06: FILE ?= $(HOME)/Downloads/jupyter0.har
06: output
	python parse.py --pattern . --ignore-cookies --with-request-type --with-response-type --file=${FILE} | tee output/06.swagger.md

07: FILE ?= $(HOME)/Downloads/jupyter1.har
07: output
	python parse.py --pattern . --ignore-cookies --with-request-type --with-response-type --file=${FILE} | tee output/07.swagger.md

