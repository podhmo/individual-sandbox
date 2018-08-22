01:
	cat 010*.json
	cat 011*.json
	dictknife diff 010*.json 011*.json
	dictknife diff 010*.json 011*.json
	dictknife diff 010*.json 011*.json -o dict
	dictknife diff 010*.json 011*.json -o dict | dictknife cat -i json -o md

02:
	cat 020*.json
	cat 021*.json
	dictknife diff 020*.json 021*.json
	dictknife diff 020*.json 021*.json
	dictknife diff 020*.json 021*.json -o dict
	dictknife diff 020*.json 021*.json -o md
	dictknife diff 020*.json 021*.json -o md --skip-empty

03:
	cat 030*.yaml
	cat 031*.yaml
	dictknife diff 030*.yaml 031*.yaml
	dictknife diff 030*.yaml 031*.yaml --unsort
	dictknife diff 030*.yaml 031*.yaml -o dict
	dictknife diff 030*.yaml 031*.yaml -o md
