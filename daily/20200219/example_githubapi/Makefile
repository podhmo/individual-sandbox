TOKEN ?= $(shell jqfpy 'get("personal_access_token")' ~/.config/cliauth/github-token.json -r )

# curl
00:
	TOKEN=$(TOKEN) bash -x -u $(shell echo $@*.bash)
# httpie
01:
	TOKEN=$(TOKEN) bash -x -u $(shell echo $@*.bash)
# organizations
02:
	TOKEN=$(TOKEN) bash -x -u $(shell echo $@*.bash)
# prs
03:
	TOKEN=$(TOKEN) bash -x -u $(shell echo $@*.bash)

04:
	echo '{"query": "$(shell cat $@*.query | sed 's/"/\\"/g' )"}' | http --json POST https://api.github.com/graphql "Authorization":"bearer ${TOKEN}"
05:
	jqfpy '{"query": "\n".join(get())}' $@*.query -i raw --slurp | http --json POST https://api.github.com/graphql "Authorization":"bearer ${TOKEN}"
