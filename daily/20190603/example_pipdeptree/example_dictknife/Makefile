00:
	dictknife cat 00people.csv -o json | jqfpy --slurp '{"people": get()}'
01:
	dictknife cat 00people.csv -o json | dictknife transform --code 'lambda d: dict(people=d)' -f json
02:
	dictknife cat 00people.csv -o json | jsonknife --debug select -i json --ref "@people"
10:
	jsonknife select --src 10ref.yaml
	jsonknife select --src 10ref.yaml --ref "#/father"
	jsonknife select --src 10ref.yaml --ref "#/father@father"
