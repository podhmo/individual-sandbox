mkdir -p dst
diff -u $1 $2 > dst/01nochange.diff
diff -u <(cat $1 | jq -S --slurp .) <(cat $2 | jq -S --slurp .) > dst/01sortkeys.diff
diff -u <(cat $1 | jq -S --slurp "sort_by(.age)") <(cat $2 | jq -S --slurp "sort_by(.age)") > dst/01sortby.diff
