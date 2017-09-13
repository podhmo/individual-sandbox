#!/bin/bash

set -eu

pids=()

{
    for i in `jot 10 1`; do echo '{"msg": "foo"}'; sleep 0.2; done
}&
pids[$!]=$!

{
    for i in `jot 10 1`; do echo '{"msg": "bar"}'; sleep 0.2; done
}&
pids[$!]=$!


wait ${pids[@]}
echo '{"msg": "ok"}'
