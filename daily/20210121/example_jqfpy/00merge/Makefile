# https://stackoverflow.com/questions/19529688/how-to-merge-2-json-objects-from-2-files-using-jq

default:
	jqfpy --slurp 'from dictknife import deepmerge; d0, d1 = get(); {"value": deepmerge(d0, d1)["value"]}' file1.json file2.json | tee actual.json
	diff -u expected.json actual.json
