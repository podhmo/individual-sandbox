# https://stackoverflow.com/questions/18592173/select-objects-based-on-value-of-variable-in-object-using-jq
default:
	jqfpy '{k:v for k,v in get().items() if v["location"] == "Stockholm"}' data.json > actual.json
	diff -u expected.json actual.json
