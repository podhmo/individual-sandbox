default: ok ng

ok2:
	dictknife diff --normalize people0.json people1.json

ok:
	jq -S "sort_by(.name)" people0.json
	bash -c 'diff -u <(jq -S "sort_by(.name)" people0.json) <(jq -S "sort_by(.name)" people1.json)'

ng:
	jq -S . people0.json
	bash -c 'diff -u <(jq -S . people0.json) <(jq -S . people1.json)'

