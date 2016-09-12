#!/bin/bash
name=`find . -type d -maxdepth 1 | sort | tail -n 1`
# cd ${name}
open https://github.com/podhmo/individual-sandbox/tree/master/daily/${name}
