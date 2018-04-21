default:
	# cat *.csv | cut -d ',' -f 1 | sort | uniq -c | sort -g
	cat *.csv | cut -d ',' -f 1 | sort -u > screenname.csv
	go run ../../20180419/example_qoquery/01list/main.go -c ~/.config/egoist/config.json --username podhmo --listname bcu30 screenname.csv

extract:
	go run 00search/main.go "#bcu30" | tee bcu30.1.csv
	for i in `seq 40`; do go run 00search/main.go "#bcu30" --max-id `tail -n 1 bcu30.$$i.csv  | cut -d ',' -f 2`  | tee bcu30.`expr $$i + 1`.csv; done
