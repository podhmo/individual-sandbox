00:
	python 00dump.py --format json person.json
	python 00dump.py --format toml person.json
	python 00dump.py --format json --extra--sort-keys person.json
	python 00dump.py --format toml --extra--sort-keys  person.json

01:
	python 01dump.py -h || echo ""
	python 01dump.py --format json --extra--sort-keys person.json
	python 01dump.py --format toml person.json

03:
	python 03dump.py -h || echo ""
	python 03dump.py --format json --extra--sort-keys person.json
	python 03dump.py --format toml --extra--sort-keys person.json
	python 03dump.py --format json --extra--sort-keys --extra--foo-bar=1 person.jso

04:
	# python 04dump.py -h || echo ""
	# python 04dump.py
	# python 04dump.py --format=csv
	python 04dump.py --format=csv --extra--fullscan
	python 04dump.py --format=json --extra--fullscan

05:
	python 05dump.py cat -h
	python 05dump.py cat --format=csv --extra--fullscan
