# export DEBUG := api

00:
	gh api graphql -f query='$(shell cat $@*.query)' | jq . | tee $@.output

# notifications
01:
	DEBUG=api gh api notifications -f per_page=100 -X GET | jq . > $@.output

02-filter := jqfpy '[{"reason": d["reason"], "title": d["subject"]["title"], "subject_type": d["subject"]["type"], "updated_at": d["updated_at"], "repository": d["repository"]["full_name"]} for d in get() if not d["repository"]["private"]]'

02:
	gh api notifications -f per_page=100 -X GET | $(02-filter) | tee $@.output.1
	gh api notifications -f page=2 -f per_page=100 -X GET | $(02-filter) | tee $@.output.2
	gh api notifications -f page=3 -f per_page=100 -X GET | $(02-filter) | tee $@.output.3

clean:
	rm -f *.output*
