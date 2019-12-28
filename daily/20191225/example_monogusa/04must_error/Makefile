gen:
	python -m monogusa.web commands.py
gen2:
	python -m monogusa.web commands.py --dst /tmp
diff: gen gen2
	diff -u web.py /tmp/web.py
# echo '{}' | http --json POST :55555/must_error
