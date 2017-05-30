mkdir -p dst
diff -u $1 $2 > dst/00nochange.diff
diff -u <(cat $1 | jq -S .) <(cat $2 | jq -S .) > dst/00sortkeys.diff
diff -u <(cat $1 | jq -S "sort_by(.age)") <(cat $2 | jq -S "sort_by(.age)") > dst/00sortby.diff
