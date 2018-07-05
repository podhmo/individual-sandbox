gen:
	bq show --format=prettyjson bigquery-public-data:samples.wikipedia > wikipedia.full.json
	bq show --schema --format=prettyjson ai_analyst_mongo:samples.wikipedia > wikipedia.json
