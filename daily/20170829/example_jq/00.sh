function loop(){
  for i in `seq $1`; do
      (>&2 echo $i time)
      cat <<EOF
  {
    "name": "foo", "no": "no.$i"
  }
EOF
  done
}

loop 5
