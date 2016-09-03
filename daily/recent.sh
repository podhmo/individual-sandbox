#!/bin/bash
cands=`find . -maxdepth 2 -mtime -7 -name readme.md | sort -r`
outfile=/tmp/recent.md

echo "" > $outfile
for i in $cands; do echo "# $i" >> $outfile; cat $i | sed 's/^#/##/g;' >> $outfile; done

cat <<EOF
generate:
$outfile
EOF
# markdown_py $outfile > ${outfile%.md}.html
# open ${outfile%.md}.html



