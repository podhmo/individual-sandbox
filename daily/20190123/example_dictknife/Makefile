run: data.csv
	dictknife cat data.csv -o md

clean:
	rm -f data.csv
data.csv:
	dictknife cat data.json --dst $@
