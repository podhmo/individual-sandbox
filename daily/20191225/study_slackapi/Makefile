00:
	pip search slack | cut -d " " -f 1 | grep -v django | tee $@.txt
	pip search slackbot | cut -d " " -f 1 | grep -v django | grep -v markov_slackbot >> $@.txt
01:
	for i in `cat 00.txt`; do pypistats recent $$i -f json; done | tee $@.json
02:
	cat 01.json | jqfpy --slurp 'sorted(get(), key=lambda x: x["data"]["last_month"], reverse=True)' --squash -c | tee $@.json
03:
	echo package,download_last_month > 03.csv
	cat 02.json | jqfpy --slurp '[print(f"""{d["package"]},{d["data"]["last_month"]}""") for d in get()]; None' >> 03.csv
04:
	dictknife cat -o md 03.csv | tee $@.md
