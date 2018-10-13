00:
	dictknife diff 2.0.yaml 3.0.yaml | tee 00.diff
01:
	dictknife diff 2.0.yaml 3.0.yaml -o md | tee 01.md
02:
	dictknife diff 2.0.yaml 3.0.yaml --skip-empty -o md | tee 02.md
03:
	bash -x -c 'jsondiff <(dictknife cat -o json 2.0.yaml) <(dictknife cat -o json 3.0.yaml)' | jq . | tee 03.json
