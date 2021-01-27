# https://stackoverflow.com/questions/36565295/jq-to-replace-text-directly-on-file-like-sed-i
default:
#	jqfpy 'd = get(); [row["properties"].__setitem__("other", "no-test") for row in d["Actions"] if row["properties"]["age"] == "3"]; d' data.json > actual.json
	jqfpy 'd = get(); [props.__setitem__("other", "no-test") for row in d["Actions"] if (props := row["properties"])["age"] == "3"]; d' data.json > actual.json
	diff -u expected.json actual.json
