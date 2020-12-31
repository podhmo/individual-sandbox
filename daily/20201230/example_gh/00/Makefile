# query
00:
	gh api graphql --paginate -f query='$(shell cat list-repository.query)' | jq . | tee repositories.json

# filtered
01:
	jqfpy --squash -c '[h.pick("nameWithOwner", "createdAt", d=row) for row in get("data/viewer/repositories/nodes") if not row["isFork"] and not row["isPrivate"] and row["createdAt"].startswith("2020-")]' repositories.json | tee 2020-repositories.json

02:
	jqfpy --squash -c '[h.pick("nameWithOwner", "updatedAt", d=row) for row in get("data/viewer/repositories/nodes") if not row["isFork"] and not row["isPrivate"] and row["updatedAt"].startswith("2020-")]' repositories.json | tee 2020-repositories.json

setup:
	port install gh # or https://github.com/cli/cli
	pip install jqfpy
