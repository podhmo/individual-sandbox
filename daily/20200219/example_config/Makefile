00:
	go run $(shell echo $@*/)main.go --config config.json | tee $(shell echo $@*/)output.txt

# overwrite
01:
	OVERWRITE_CONFIG=overwrite.json go run $(shell echo $@*/)main.go --config config.json | tee $(shell echo $@*/)output.txt

02:
	diff -u $(shell echo 00*/)output.txt $(shell echo 01*/)output.txt > $@.diff || exit 0

# config (python)
03:
	python $(shell echo $@*/)main.py --config config.json | tee $(shell echo $@*/)output.txt

# overwrite (python)
04:
	OVERWRITE_CONFIG=overwrite.json python $(shell echo $@*/)main.py --config config.json | tee $(shell echo $@*/)output.txt

05:
	diff -u $(shell echo 03*/)output.txt $(shell echo 04*/)output.txt > $@.diff || exit 0
