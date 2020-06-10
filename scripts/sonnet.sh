#!/bin/bash
for INPUT in "$@"
do
stack exec -- seq-sel $INPUT --config=poem-config/sonnet.yaml \
> output/$INPUT-sonnet.txt
cat output/$INPUT-sonnet.txt
done
