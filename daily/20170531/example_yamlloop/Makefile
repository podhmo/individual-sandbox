00:
	dictknife concat apps.yaml --output-format json | jq -r '.apps | map(select(.use)) | sort_by(.name) | .[].name'

01:
	dictknife concat apps.yaml --output-format json | jq -r '.apps | keys as $$k | .[].use as $$v | map($$k)'

hmm:
	cat t.json | jq -r '. as $$in| keys[] | [.]+$$in[.].a | map(tostring+" ") | add'
