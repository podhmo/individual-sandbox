set-debug:
	@echo BASH_COMP_DEBUG_FILE=$(shell pwd)/debug-file

# help
00:
	gh help completion

# comp gen
01:
	gh completion -s bash > bash_completion

# query
02:
	gh api graphql --paginate -f query='$(shell cat list-repository.query)' | jq . | tee repositories.json

# filtered
03:
	jqfpy --squash -c '[h.pick("nameWithOwner", "createdAt", d=row) for row in get("data/viewer/repositories/nodes") if not row["isFork"] and not row["isPrivate"] and row["createdAt"].startswith("2020-")]' repositories.json | tee 2020-repositories.json
