#!/bin/bash
for INPUT in "$@"
do
stack exec -- seq-sel $INPUT --config=poem-config/limerick.yaml \
> output/$INPUT-limerick.txt
cat output/$INPUT-limerick.txt
done
