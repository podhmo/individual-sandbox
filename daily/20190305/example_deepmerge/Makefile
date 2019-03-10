00:
	dictknife mkdict name=foo age=20 | tee person.json
	dictknife mkdict nickname=F | tee nickname.json
	dictknife cat person.json nickname.json -o json

01:
	dictknife mkdict name=FOO nickname=F | tee nickname2.json
	dictknife cat person.json nickname2.json -o json

02:
	dictknife mkdict father/name=bar mother/name=boo | tee parents.json
	dictknife mkdict mother/name=xyz | tee parents2.json
	dictknife cat person.json parents.json parents2.json -o json

03:
	dictknife --log=WARNING --compact mkdict vs/=1 vs/=2 vs/=3 vs/=4 vs/=5 | tee xs.json
	dictknife --log=WARNING --compact mkdict vs/=2 vs/=4 vs/=6 | tee ys.json
	dictknife --log=WARNING --compact cat xs.json ys.json -o json

04:
	dictknife --log=WARNING --compact mkdict vs/=1 vs/=2 vs/=3 vs/=4 vs/=5 | tee xs.json
	dictknife --log=WARNING --compact mkdict vs/=2 vs/=4 vs/=6 | tee ys.json
	dictknife --log=WARNING --compact cat --merge-method=replace xs.json ys.json -o json

05:
	dictknife --log=WARNING --compact mkdict vs/=1 vs/=2 vs/=3 vs/=4 vs/=5 | tee xs.json
	dictknife --log=WARNING --compact mkdict vs/=2 vs/=4 vs/=6 | tee ys.json
	dictknife --log=WARNING --compact cat --merge-method=append xs.json ys.json -o json

06:
	cat values.json
	cat values2.json
	dictknife cat --merge-method=merge values.json values2.json -o json
