00:
	python 00*

01:
	@dictknife transform --function "./01transform.py:names" --src app.yaml --output-format raw

# 02:
# 	python 02loop.py --iterator "<>['X'].values()" --project 'snakecase(<>["name"])' --reduce '"\n".join(<>)'

03:
	dictknife concat app.yaml --output-format json | jq -r ".X[].name"

04:
	dictknife transform --src app.yaml --code 'lambda d: "\n".join(__import__("zenmai.naming", fromlist=1).snakecase(v["name"]) for v in d["X"].values())' --output-format raw

05:
	dictknife transform --src app.yaml --use "zenmai.naming:snakecase" --code 'lambda d: "\n".join(snakecase(v["name"]) for v in d["X"].values())'
