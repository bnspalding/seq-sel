#!/bin/bash
for INPUT in "$@"
do
stack exec -- seq-sel $INPUT --config=poem-config/couplets.yaml \
> output/$INPUT-couplets.txt
cat output/$INPUT-couplets.txt
done
