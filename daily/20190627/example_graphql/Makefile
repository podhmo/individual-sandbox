ACCESS_TOKEN := xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

00:
	cat 01*.json | http -b --json POST https://api.github.com/graphql Authorization:"token ${ACCESS_TOKEN}" | jqfpy 'get("data/viewer/repositories/nodes")' --squash -c

01:
	cat 01*.json | http -a podhmo -b --json POST https://api.github.com/graphql
