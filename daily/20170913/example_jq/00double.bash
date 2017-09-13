#!/bin/bash

set -eu

jq . --unbuffered <(bash -c 'for i in `jot 10 1`; do echo "{\"name\": \"foo\"}"; sleep 0.2; done') <(bash -c 'for i in `jot 10 1`; do echo "{\"name\": \"bar\"}"; sleep 0.2; done')

