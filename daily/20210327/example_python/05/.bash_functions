## colorful word level highlight diff

# diff -u <> <> | cdiffhighlight
cdiffhighlight() {
  SED=$(which gsed || which sed)
  $SED 's/^- /\x1b[1;31m-/;s/^+ /\x1b[1;32m+/;s/^@/\x1b[1;36m@/;s/$/\x1b[0m/' | diff-highlight
}
