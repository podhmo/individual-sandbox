00:
	cat data.txt | jqfpy -i raw 'f"@ {get()} @"'
01:
	cat data.txt | jqfpy -r -i raw 'f"@ {get()} @"'
02:
	cat data.txt | jqfpy -r -i raw '[x.strip() for x in get().split("::")]' -c


10:
	cat data.txt | jqfpy -r -i raw 'None'
11:
	for i in $$(seq 10); do echo "{\"val\": $$i}"; sleep 0.1; done | jqfpy -u
12:
	for i in $$(seq 10); do echo "{\"val\": $$i}"; sleep 0.1; done | jqfpy -i raw
13:
	for i in $$(seq 10); do echo "{\"val\": $$i}"; sleep 0.1; done | jqfpy -u -i raw
14:
	for i in $$(seq 10); do echo "{\"val\": $$i}"; sleep 0.1; done | jqfpy -u -i raw --slurp

20:
	jqfpy --slurp '[get()]' x.json y.json
