#!/bin/bash

name=`date +%Y%m%d`/example_go
if [[ ! -d ${name} ]]; then
  echo mkdir -p ${name} >&2
  mkdir -p ${name}
fi
if [[ ! -f ${name}/go.mod ]]; then 
  echo go mod init >&2
  cd ${name}
  go mod init github.com/podhmo/individual-sandbox/daily/${name}
  cd ../../../
  go work use ./daily/${name} 
fi
cd ${name}
