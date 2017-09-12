default:
	cat 00data.txt | jqfpy --slurp 'sum(get())'

jq:
	cat 00data.txt | jq --slurp 'add'

past:
	cat 00data.txt | python -c 'import sys; print(sum(float(line.strip()) for line in sys.stdin))'
