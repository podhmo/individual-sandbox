00:
	cat 00person.json | dictknife transform --fn flatten -i json -o csv
	dictknife --flatten cat -o csv 00person.json
	dictknife cat -o csv 00person.json
	dictknife --flatten cat -o json 00person.json
	dictknife cat -o json 00person.json
01:
	dictknife mkdict xs/0 1 xs/1 3 xs/2 5
	dictknife mkdict xs/ 1 xs/ 3 xs/ 5
	dictknife mkdict xs/-1 1 xs/-1 3 xs/-1 5
02:
	dictknife mkdict \
	query/bool/must/0/terms/jobId/ 5bd280d5493ad70001dda105 \
	query/bool/must/0/terms/jobId/ 5bd280b7493ad70001dda100 \
	query/bool/must/0/terms/jobId/ 5bd1b0fc493ad70001dd94a3 \
	query/bool/must/0/terms/jobId/ 5bd184e5493ad70001dd948c \
	size 1000
03:
	dictknife mkdict --name foo --age 20 ";" --name bar
	dictknife mkdict --squash --name foo --age 20 ";" --name bar
	dictknife --compact mkdict --squash --name foo --age 20 ";" --name bar
04:
	dictknife cat 04*.json -i json -o json
	dictknife --unescape cat 04*.json -f json
